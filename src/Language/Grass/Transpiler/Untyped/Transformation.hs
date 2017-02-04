module Language.Grass.Transpiler.Untyped.Transformation
    ( Transf
    , aNormalize
    , elimVars
    , liftLambdas
    , normalize
    , DefInfo (..)
    , transfDef
    , transfDefs
    , CharSet (..)
    , defaultCharSet
    , wideCharSet
    , plantTerm
    , plantDefs
    , plant
    ) where

import Control.Monad.Except

import Language.Grass.Transpiler.Untyped.Term
import Language.Grass.Transpiler.Untyped.Optimization


type Transf a = Except String a

throwErrorWithPos :: Show p => p -> String -> Transf a
throwErrorWithPos pos msg = throwError $ "Error at " ++ show pos ++ ":\n" ++ msg


-- A-normalize (+ eliminate applications with abstractions)
data EvalCtx =
      EmptyCtx
    | LetValCtx IxTerm

applyCtx :: EvalCtx -> IxTerm -> IxTerm
applyCtx EmptyCtx x      = x
applyCtx (LetValCtx y) x = IxLet x y

shiftCtx :: Int -> Int -> EvalCtx -> EvalCtx
shiftCtx _ _ EmptyCtx      = EmptyCtx
shiftCtx i n (LetValCtx y) = LetValCtx (shift (i + 1) n y)

aNormalize' :: IxTerm -> EvalCtx -> IxTerm
aNormalize' x@(IxVar _) ctx = applyCtx ctx x
aNormalize' (IxAbs x)   ctx = applyCtx ctx $ IxAbs (aNormalize x)
aNormalize' (IxApp x y) ctx = aNormalize' x ctx1
    where
        ctx1 = LetValCtx (aNormalize' (shift 0 1 y) ctx2)
        ctx2 = LetValCtx (applyCtx (shiftCtx 0 2 ctx) $ IxApp (IxVar 1) (IxVar 0))
aNormalize' (IxLet x y) ctx = aNormalize' x ctx'
    where
        ctx' = LetValCtx (aNormalize' y (shiftCtx 0 1 ctx))

aNormalize :: IxTerm -> IxTerm
aNormalize x = aNormalize' x EmptyCtx


-- simplify + eliminate isolated variables but 0
elimVars' :: IxTerm -> IxTerm
elimVars' x@(IxVar 0)           = x
elimVars' x@(IxVar _)           = IxLet (IxAbs (IxVar 0)) (IxApp (IxVar 0) (shift 0 1 x))
elimVars' (IxAbs x)             = IxAbs (elimVars' x)
elimVars' x@(IxApp _ _)         = x
elimVars' (IxLet x (IxVar 0))   = x
elimVars' (IxLet x@(IxVar _) y) = elimVars' $ shift 0 (-1) (subst 0 (shift 0 1 x) y)
elimVars' (IxLet x y)           = IxLet (elimVars' x) (elimVars y)

elimVars :: IxTerm -> IxTerm
elimVars x@(IxVar _) = IxLet (IxAbs (IxVar 0)) (IxApp (IxVar 0) (shift 0 1 x))
elimVars x           = elimVars' x


-- lambda-lifting
liftLambdas' :: IxTerm -> IxTerm
liftLambdas' x@(IxVar _) = x
liftLambdas' (IxAbs x) = case liftLambdas' x of
    IxLet s@(IxAbs _) t
        | s `contains` 0 -> IxLet (IxAbs s)
            (liftLambdas' $ IxAbs
                (IxLet (IxApp (IxVar 1) (IxVar 0))
                    (shift 2 1 t)
                )
            )
        | otherwise -> IxLet (shift 0 (-1) s)
            (liftLambdas' $ IxAbs (swap 0 1 t))
    x' -> IxAbs x'
liftLambdas' x@(IxApp _ _) = x
liftLambdas' (IxLet x y) = case liftLambdas' x of
    IxLet s t    -> liftLambdas' $ IxLet s (IxLet t (shift 1 1 y))
    x'@(IxAbs _) -> case liftLambdas' y of
        y'@(IxAbs _) -> liftLambdas' $ IxLet x'
            (IxLet y'
                (IxLet (IxAbs (IxVar 0))
                    (IxApp (IxVar 0) (IxVar 1))
                )
            )
        y' -> IxLet x' y'
    x' -> case liftLambdas' y of
        IxLet u@(IxAbs _) v
            | u `contains` 0 -> IxLet (IxAbs u)
                (liftLambdas' $ IxLet (shift 0 1 x')
                    (IxLet (IxApp (IxVar 1) (IxVar 0))
                        (shift 2 1 v)
                    )
                )
            | otherwise -> IxLet (shift 0 (-1) u)
                (liftLambdas' $ IxLet (shift 0 1 x')
                    (swap 0 1 v)
                )
        y'@(IxAbs _) -> liftLambdas' $ IxLet x'
            (IxLet y'
                (IxLet (IxAbs (IxVar 0))
                    (IxApp (IxVar 0) (IxVar 1))
                )
            )
        y' -> IxLet x' y'

liftLambdas :: IxTerm -> IxTerm
liftLambdas (IxLet x y) = case liftLambdas x of
    IxLet s t    -> liftLambdas $ IxLet s (IxLet t (shift 1 1 y))
    x'@(IxAbs _) -> IxLet x' (liftLambdas y)
    x'           -> IxLet x' (liftLambdas y)
liftLambdas x = liftLambdas' x


-- full normalization
normalize :: IxTerm -> IxTerm
normalize = liftLambdas . elimVars . aNormalize


-- transform definitions
data DefInfo = DefInfo String Int

findIndex :: [DefInfo] -> String -> Maybe Int
findIndex ctx name = walk 0 ctx
    where
        walk _ [] = Nothing
        walk i (DefInfo defName defSize : rest)
            | name == defName = Just i
            | otherwise       = walk (i + defSize) rest

toIndexed :: [DefInfo] -> Term -> Transf IxTerm
toIndexed ctx (Var pos name) = case findIndex ctx name of
    Just i  -> return $ IxVar i
    Nothing -> throwErrorWithPos pos $ "unbound variable `" ++ name ++ "'"
toIndexed ctx (Abs _ param x) = do
    x' <- toIndexed (DefInfo param 1 : ctx) x
    return $ IxAbs x'
toIndexed ctx (App _ x y) = do
    x' <- toIndexed ctx x
    y' <- toIndexed ctx y
    return $ IxApp x' y'
toIndexed ctx (Let _ name x y) = do
    x' <- toIndexed ctx x
    y' <- toIndexed (DefInfo name 1 : ctx) y
    return $ IxLet x' y'

height :: IxTerm -> Int
height x = walk 1 x
    where
        walk n (IxLet _ y) = walk (n + 1) y
        walk n _           = n

glue :: IxTerm -> IxTerm -> IxTerm
glue (IxLet x y) z = IxLet x (glue y z)
glue x z           = IxLet x z

transfDef :: [DefInfo] -> Def -> Optimizer -> Transf (DefInfo, IxTerm)
transfDef ctx (Def _ name term) opt = do
    x <- toIndexed ctx term
    let x'   = localOpt opt $ normalize x
    let size = height x'
    return (DefInfo name size, x')

transfDefs :: [DefInfo] -> [Def] -> Optimizer -> Transf IxTerm
transfDefs ctx defs opt = globalOpt opt <$> walk ctx defs
    where
        walk _ [] = throwError "no definitions"
        walk cs (d : []) = do
            (_, x) <- transfDef cs d opt
            return x
        walk cs (d : ds) = do
            (c, x) <- transfDef cs d opt
            glue x <$> walk (c : cs) ds


-- planting
data CharSet = CharSet { lowerW :: Char
                       , upperW :: Char
                       , lowerV :: Char
                       }

defaultCharSet :: CharSet
defaultCharSet = CharSet { lowerW = 'w', upperW = 'W', lowerV = 'v' }

wideCharSet :: CharSet
wideCharSet = CharSet { lowerW = '\xFF57', upperW = '\xFF37', lowerV = '\xFF56' }

plantTerm' :: CharSet -> IxTerm -> Transf String
plantTerm'  _ (IxVar _)                   = throwError "unexpected variable"
plantTerm' cs (IxAbs (IxVar 0))           = return $ [lowerW cs]
plantTerm' cs (IxAbs x)                   = (lowerW cs :) <$> plantTerm cs x
plantTerm' cs (IxApp (IxVar i) (IxVar j)) = return $ replicate (i + 1) (upperW cs) ++ replicate (j + 1) (lowerW cs)
plantTerm'  _ (IxApp _ _)                 = throwError "unexpected application form"
plantTerm'  _ (IxLet _ _)                 = throwError "unexpected binding"

plantTerm :: CharSet -> IxTerm -> Transf String
plantTerm cs (IxLet x@(IxApp _ _) y@(IxApp _ _))           = (++) <$> plantTerm' cs x <*> plantTerm' cs y
plantTerm cs (IxLet x@(IxApp _ _) y@(IxLet (IxApp _ _) _)) = (++) <$> plantTerm' cs x <*> plantTerm cs y
plantTerm cs (IxLet x y)                                   = (\s t -> s ++ lowerV cs : t) <$> plantTerm' cs x <*> plantTerm cs y
plantTerm cs x                                             = plantTerm' cs x

plantDefs :: [DefInfo] -> [Def] -> Optimizer -> CharSet -> Transf String
plantDefs ctx defs opt cs = do
    term <- transfDefs ctx defs opt
    case term of
        IxApp _ _           -> plantTerm cs (prefix term)
        IxLet (IxApp _ _) _ -> plantTerm cs (prefix term)
        _                   -> plantTerm cs term
    where
        prefix term = IxLet (IxAbs (IxVar 0)) (shift 0 1 term)

plant :: [DefInfo] -> [Def] -> Optimizer -> CharSet -> Either String String
plant ctx defs opt cs = runExcept $ plantDefs ctx defs opt cs

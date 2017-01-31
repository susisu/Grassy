module Language.Grass.Transpiler.Untyped.Transformation
    ( DefInfo (..)
    , toIndexed
    , aNormalize
    , elimVars
    , liftLambdas
    , normalize
    ) where

import Control.Monad.Except

import Language.Grass.Transpiler.Untyped.Term


type Transf a = Except String a

throwErrorWithPos :: Show p => p -> String -> Transf a
throwErrorWithPos pos msg = throwError $ "Error at " ++ show pos ++ ":\n" ++ msg


-- operations on indexed terms
shift :: Int -> Int -> IxTerm -> IxTerm
shift i n x@(IxVar j)
    | j >= i    = IxVar (j + n)
    | otherwise = x
shift i n (IxAbs x)   = IxAbs (shift (i + 1) n x)
shift i n (IxApp x y) = IxApp (shift i n x) (shift i n y)
shift i n (IxLet x y) = IxLet (shift i n x) (shift (i + 1) n y)

subst :: Int -> IxTerm -> IxTerm -> IxTerm
subst i s x@(IxVar j)
    | j == i    = s
    | otherwise = x
subst i s (IxAbs x)   = IxAbs (subst (i + 1) (shift 0 1 s) x)
subst i s (IxApp x y) = IxApp (subst i s x) (subst i s y)
subst i s (IxLet x y) = IxLet (subst i s x) (subst (i + 1) (shift 0 1 s) y)

swap :: Int -> Int -> IxTerm -> IxTerm
swap i j x@(IxVar k)
    | i == k    = IxVar j
    | j == k    = IxVar i
    | otherwise = x
swap i j (IxAbs x)   = IxAbs (swap (i + 1) (j + 1) x)
swap i j (IxApp x y) = IxApp (swap i j x) (swap i j y)
swap i j (IxLet x y) = IxLet (swap i j x) (swap (i + 1) (j + 1) y)

contains :: IxTerm -> Int -> Bool
IxVar j   `contains` i = i == j
IxAbs x   `contains` i = x `contains` (i + 1)
IxApp x y `contains` i = (x `contains` i) || (y `contains` i)
IxLet x y `contains` i = (x `contains` i) || (y `contains` (i + 1))


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
elimVars' (IxLet x y)           = IxLet x (elimVars' y)

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

serialize :: IxTerm -> [IxTerm]
serialize (IxLet x y) = x : serialize y
serialize x           = [x]

transfDef :: [DefInfo] -> Def -> Transf (DefInfo, [IxTerm])
transfDef ctx (Def _ name term) = do
    x <- toIndexed ctx term
    let xs   = serialize $ normalize x
    let size = length xs
    return (DefInfo name size, xs)

transfDefs :: [DefInfo] -> [Def] -> Transf [IxTerm]
transfDefs _ []             = return []
transfDefs ctx (def : defs) = do
    (info, xs) <- transfDef ctx def
    rest       <- transfDefs (info : ctx) defs
    return $ xs ++ rest

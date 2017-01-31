module Language.Grass.Transpiler.Untyped.Transformation
    ( DefInfo (..)
    , toIndexed
    , normalize
    ) where

import Control.Monad.Except

import Language.Grass.Transpiler.Untyped.Term


type Transf a = Except String a

throwErrorWithPos :: Show p => p -> String -> Transf a
throwErrorWithPos pos msg = throwError $ "Error at " ++ show pos ++ ":\n" ++ msg


-- convert term to de Bruijn indexed term
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


-- A-normalization
data EvalCtx =
      EmptyCtx
    | LetValCtx IxTerm

applyCtx :: EvalCtx -> IxTerm -> IxTerm
applyCtx EmptyCtx x      = x
applyCtx (LetValCtx y) x = IxLet x y

shiftCtx :: Int -> Int -> EvalCtx -> EvalCtx
shiftCtx _ _ EmptyCtx      = EmptyCtx
shiftCtx i n (LetValCtx y) = LetValCtx (shift (i + 1) n y)

normalizeWithCtx :: IxTerm -> EvalCtx -> IxTerm
normalizeWithCtx x@(IxVar _) ctx = applyCtx ctx x
normalizeWithCtx (IxAbs x)   ctx = applyCtx ctx $ IxAbs (normalize x)
normalizeWithCtx (IxApp x y) ctx = normalizeWithCtx x ctx1
    where
        ctx1 = LetValCtx (normalizeWithCtx (shift 0 1 y) ctx2)
        ctx2 = LetValCtx (applyCtx (shiftCtx 0 2 ctx) $ IxApp (IxVar 1) (IxVar 0))
normalizeWithCtx (IxLet x y) ctx = normalizeWithCtx x ctx'
    where
        ctx' = LetValCtx (normalizeWithCtx y (shiftCtx 0 1 ctx))

simplify :: IxTerm -> IxTerm
simplify (IxLet x@(IxVar _) y) = simplify (shift 0 (-1) $ subst 0 (shift 0 1 x) y)
simplify (IxLet x y)           = IxLet x (simplify y)
simplify x                     = x

normalize :: IxTerm -> IxTerm
normalize x = simplify $ normalizeWithCtx x EmptyCtx

module Language.Grass.Transpiler.Untyped.Transformation
    ( toIndexed
    ) where

import Control.Monad.Except
import Data.List

import Language.Grass.Transpiler.Untyped.Term


type Transf a = Except String a

throwErrorWithPos :: Show p => p -> String -> Transf a
throwErrorWithPos pos msg = throwError $ "Error at " ++ show pos ++ ":\n" ++ msg


-- convert term to de Bruijn indexed term
toIndexed :: [String] -> Term -> Transf IxTerm
toIndexed ctx (Var pos name) = case elemIndex name ctx of
    Just i  -> return $ IxVar i
    Nothing -> throwErrorWithPos pos $ "unbound variable `" ++ name ++ "'"
toIndexed ctx (Abs _ param x) = do
    x' <- toIndexed (param : ctx) x
    return $ IxAbs x'
toIndexed ctx (Let _ name x y) = do
    x' <- toIndexed ctx x
    y' <- toIndexed (name : ctx) y
    return $ IxLet x' y'
toIndexed ctx (App _ x y) = do
    x' <- toIndexed ctx x
    y' <- toIndexed ctx y
    return $ IxApp x' y'

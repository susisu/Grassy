module Language.Grass.Transpiler.Untyped.Transformation
    ( DefInfo (..)
    , toIndexed
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
toIndexed ctx (Let _ name x y) = do
    x' <- toIndexed ctx x
    y' <- toIndexed (DefInfo name 1 : ctx) y
    return $ IxLet x' y'
toIndexed ctx (App _ x y) = do
    x' <- toIndexed ctx x
    y' <- toIndexed ctx y
    return $ IxApp x' y'

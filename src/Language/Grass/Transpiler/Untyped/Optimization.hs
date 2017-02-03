module Language.Grass.Transpiler.Untyped.Optimization
    ( elimUnused
    , elimDuplicate
    ) where

import Data.List

import Language.Grass.Transpiler.Untyped.Term

elimUnused :: IxTerm -> IxTerm
elimUnused (IxLet x@(IxVar _) y)
    | y `contains` 0 = IxLet x (elimUnused y)
    | otherwise      = shift 0 (-1) y
elimUnused (IxLet x@(IxAbs _) y)
    | y `contains` 0 = IxLet x (elimUnused y)
    | otherwise      = shift 0 (-1) y
elimUnused x = x

elimDuplicate :: IxTerm -> IxTerm
elimDuplicate x = walk [] x
    where
        walk ctx (IxLet u@(IxVar _) v) = walk ctx $ shift 0 (-1) (subst 0 (shift 0 1 u) v)
        walk ctx (IxLet u@(IxAbs _) v) = case elemIndex u ctx of
            Just i  -> walk ctx $ shift 0 (-1) (subst 0 (IxVar (i + 1)) v)
            Nothing -> IxLet u (walk (map (shift 0 1) $ u : ctx) v)
        walk ctx (IxLet u v) = IxLet u (walk (map (shift 0 1) ctx) v)
        walk _ u = u

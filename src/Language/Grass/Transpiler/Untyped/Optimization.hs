module Language.Grass.Transpiler.Untyped.Optimization
    ( Optimizer (..)
    , noOpt
    , elimUnused
    , elimDuplicate
    ) where

import Data.List

import Language.Grass.Transpiler.Untyped.Term


data Optimizer = Optimizer { globalOpt :: IxTerm -> IxTerm
                           , localOpt :: IxTerm -> IxTerm
                           }

noOpt :: Optimizer
noOpt = Optimizer { globalOpt = id, localOpt = id }


elimUnused :: IxTerm -> IxTerm
elimUnused x = walk x id
    where
        walk (IxLet u@(IxVar _) v) k = walk v $ \v' ->
            if v' `contains` 0
                then k (IxLet u v')
                else k (shift 0 (-1) v')
        walk (IxLet u@(IxAbs _) v) k = walk v $ \v' ->
            if v' `contains` 0
                then k (IxLet u v')
                else k (shift 0 (-1) v')
        walk (IxLet u v) k = walk v $ \v' -> k (IxLet u v')
        walk u k = k u

elimDuplicate :: IxTerm -> IxTerm
elimDuplicate x = walk [] x
    where
        walk ctx (IxLet u@(IxVar _) v) = walk ctx $ shift 0 (-1) (subst 0 (shift 0 1 u) v)
        walk ctx (IxLet u@(IxAbs _) v) = case elemIndex u ctx of
            Just i  -> walk ctx $ shift 0 (-1) (subst 0 (IxVar (i + 1)) v)
            Nothing -> IxLet u (walk (map (shift 0 1) $ u : ctx) v)
        walk ctx (IxLet u v) = IxLet u (walk (map (shift 0 1) $ IxVar (-1) : ctx) v) -- IxVar (-1) is dummy
        walk _ u = u

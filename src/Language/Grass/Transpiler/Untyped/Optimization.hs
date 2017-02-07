module Language.Grass.Transpiler.Untyped.Optimization
    ( Optimizer (..)
    , noOpt
    , elimUnused
    , elimUnused'
    , elimDuplicate
    , elimDuplicate'
    ) where

import Data.List
import Safe

import Language.Grass.Transpiler.Untyped.Term


data Optimizer = Optimizer { globalOpt :: IxTerm -> IxTerm
                           , localOpt :: IxTerm -> IxTerm
                           }

noOpt :: Optimizer
noOpt = Optimizer { globalOpt = id, localOpt = id }


-- `effs' must specify ALL potential side-effects
elimUnused :: [Bool] -> IxTerm -> IxTerm
elimUnused effs x = walk effs x id
    where
        walk es (IxLet u@(IxVar i) v) k =
            let e = atDef False es i
            in walk (e : es) v $ \v' ->
                if v' `contains` 0
                    then k (IxLet u v')
                    else k (shift 0 (-1) v')
        walk es (IxLet u@(IxAbs _) v) k =
            let e = or $ zipWith (&&) es (map (u `contains`) [0..])
            in walk (e : es) v $ \v' ->
                if v' `contains` 0
                    then k (IxLet u v')
                    else k (shift 0 (-1) v')
        walk es (IxLet u@(IxApp (IxVar i) (IxVar j)) v) k =
            if atDef False es i || atDef False es j
                then walk (True : es) v $ \v' -> k (IxLet u v')
                else walk (False : es) v $ \v' ->
                    if v' `contains` 0
                        then k (IxLet u v')
                        else k (shift 0 (-1) v')
        walk es (IxLet u v) k = walk (True : es) v $ \v' -> k (IxLet u v')
        walk _ u k = k u

elimUnused' :: IxTerm -> IxTerm
elimUnused' x = walk x id
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

-- `effs' must specify ALL potential side-effects
elimDuplicate :: [Bool] -> IxTerm -> IxTerm
elimDuplicate effs x = walk effs [] x
    where
        walk es cs (IxLet u@(IxVar _) v) = walk es cs $ shift 0 (-1) (subst 0 (shift 0 1 u) v)
        walk es cs (IxLet u@(IxAbs _) v) = case elemIndex u cs of
            Just i  -> walk es cs $ shift 0 (-1) (subst 0 (IxVar (i + 1)) v)
            Nothing ->
                let e = or $ zipWith (&&) es (map (u `contains`) [0..])
                in IxLet u (walk (e : es) (map (shift 0 1) $ u : cs) v)
        walk es cs (IxLet u@(IxApp (IxVar i) (IxVar j)) v) =
            if atDef False es i || atDef False es j
                then IxLet u (walk (True : es) (map (shift 0 1) $ u : cs) v)
                else case elemIndex u cs of
                    Just k  -> walk es cs $ shift 0 (-1) (subst 0 (IxVar (k + 1)) v)
                    Nothing -> IxLet u (walk (False : es) (map (shift 0 1) $ u : cs) v)
        walk es cs (IxLet u v) = IxLet u (walk (True : es) (map (shift 0 1) $ u : cs) v)
        walk _ _ u = u

elimDuplicate' :: IxTerm -> IxTerm
elimDuplicate' x = walk [] x
    where
        walk ctx (IxLet u@(IxVar _) v) = walk ctx $ shift 0 (-1) (subst 0 (shift 0 1 u) v)
        walk ctx (IxLet u@(IxAbs _) v) = case elemIndex u ctx of
            Just i  -> walk ctx $ shift 0 (-1) (subst 0 (IxVar (i + 1)) v)
            Nothing -> IxLet u (walk (map (shift 0 1) $ u : ctx) v)
        walk ctx (IxLet u v) = IxLet u (walk (map (shift 0 1) $ IxVar (-1) : ctx) v) -- IxVar (-1) is dummy
        walk _ u = u

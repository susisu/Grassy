module Language.Grass.Transpiler.Untyped.Optimization (
    Optimizer (..)
  , noOpt
  , Eff (..)
  , elimUnused
  , elimUnused'
  , elimDuplicate
  , elimDuplicate'
  ) where

import Data.List
import Data.Monoid
import Safe

import Language.Grass.Transpiler.Untyped.Term


data Optimizer = Optimizer {
    globalOpt :: IxTerm -> IxTerm
  , localOpt  :: IxTerm -> IxTerm
  }

-- no optimization
noOpt :: Optimizer
noOpt = Optimizer { globalOpt = id, localOpt = id }

-- flag for potential side-effects
data Eff = NoEff | Eff

instance Monoid Eff where
  mempty = NoEff
  mappend NoEff NoEff = NoEff
  mappend _ _         = Eff

-- eliminate unused definitions
-- `effs' must specify ALL potential side-effects
elimUnused :: [Eff] -> IxTerm -> IxTerm
elimUnused effs x = walk effs x id
  where
    walk es (IxLet u@(IxVar i) v) k =
        let e = atDef Eff es i in
        walk (e : es) v $ \v' ->
          if v' `contains` 0 then
            k (IxLet u v')
          else
            -- remove binding if unused
            k (shift 0 (-1) v')
    walk es (IxLet u@(IxAbs _) v) k =
        let
          ms = map (u `contains`) [0..]
          e = mconcat $ zipWith (\e' m -> if m then e' else NoEff) es ms
        in
        walk (e : es) v $ \v' ->
          if v' `contains` 0 then
            k (IxLet u v')
          else
            -- remove binding if unused
            k (shift 0 (-1) v')
    walk es (IxLet u@(IxApp (IxVar i) (IxVar j)) v) k =
        case atDef Eff es i <> atDef Eff es j of
          Eff ->
            walk (Eff : es) v $ \v' -> k (IxLet u v')
          NoEff ->
            walk (NoEff : es) v $ \v' ->
              if v' `contains` 0 then
                k (IxLet u v')
              else
                -- remove binding if unused
                k (shift 0 (-1) v')
    walk es (IxLet u v) k =
        -- unknown; keep intact
        walk (Eff : es) v $ \v' -> k (IxLet u v')
    walk _ u k = k u

-- eliminate unused definitions
-- assuming all functions potentailly have side-effects
elimUnused' :: IxTerm -> IxTerm
elimUnused' x = walk x id
  where
    walk (IxLet u@(IxVar _) v) k =
        walk v $ \v' ->
          if v' `contains` 0 then
            k (IxLet u v')
          else
            -- remove binding if unused
            k (shift 0 (-1) v')
    walk (IxLet u@(IxAbs _) v) k =
        walk v $ \v' ->
          if v' `contains` 0 then
            k (IxLet u v')
          else
            -- remove binding if unused
            k (shift 0 (-1) v')
    walk (IxLet u v) k = walk v $ \v' -> k (IxLet u v')
    walk u k = k u

-- eliminate duplicate definitions
-- `effs' must specify ALL potential side-effects
elimDuplicate :: [Eff] -> IxTerm -> IxTerm
elimDuplicate effs = walk effs []
  where
    walk es cs (IxLet u@(IxVar _) v) =
        -- rename
        walk es cs $ shift 0 (-1) (subst 0 (shift 0 1 u) v)
    walk es cs (IxLet u@(IxAbs _) v) =
        case elemIndex u cs of
          Just i ->
            -- reuse the already existing one
            walk es cs $ shift 0 (-1) (subst 0 (IxVar (i + 1)) v)
          Nothing ->
            let
              ms = map (u `contains`) [0..]
              e = mconcat $ zipWith (\e' m -> if m then e' else NoEff) es ms
            in
            let v' = walk (e : es) (map (shift 0 1) $ u : cs) v in
            IxLet u v'
    walk es cs (IxLet u@(IxApp (IxVar i) (IxVar j)) v) =
        case atDef Eff es i <> atDef Eff es j of
          Eff -> IxLet u (walk (Eff : es) (map (shift 0 1) $ u : cs) v)
          NoEff ->
            case elemIndex u cs of
              Just k ->
                -- reuse the already existing one
                walk es cs $ shift 0 (-1) (subst 0 (IxVar (k + 1)) v)
              Nothing ->
                let v' = walk (NoEff : es) (map (shift 0 1) $ u : cs) v in
                IxLet u v'
    walk es cs (IxLet u v) =
        -- unknown; keep intact
        let v' = walk (Eff : es) (map (shift 0 1) $ u : cs) v in
        IxLet u v'
    walk _ _ u = u

-- eliminate duplicate definitions
-- assuming all functions potentailly have side-effects
elimDuplicate' :: IxTerm -> IxTerm
elimDuplicate' = walk []
  where
    walk ctx (IxLet u@(IxVar _) v) =
        -- rename
        walk ctx $ shift 0 (-1) (subst 0 (shift 0 1 u) v)
    walk ctx (IxLet u@(IxAbs _) v) =
        case elemIndex u ctx of
          Just i ->
            -- reuse the already existing one
            walk ctx $ shift 0 (-1) (subst 0 (IxVar (i + 1)) v)
          Nothing ->
            let v' = walk (map (shift 0 1) $ u : ctx) v in
            IxLet u v'
    walk ctx (IxLet u v) =
        -- IxVar (-1) is dummy, not equals with any valid terms
        let v' = walk (map (shift 0 1) $ IxVar (-1) : ctx) v in
        IxLet u v'
    walk _ u = u

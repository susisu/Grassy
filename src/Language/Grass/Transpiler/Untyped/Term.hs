module Language.Grass.Transpiler.Untyped.Term
    ( Term (..)
    , Def (..)
    , IxTerm (..)
    , shift
    , subst
    , swap
    , contains
    ) where

import Text.Parsec.Pos (SourcePos)


-- terms
data Term =
      Var SourcePos String
    | Abs SourcePos String Term
    | App SourcePos Term Term
    | Let SourcePos String Term Term

instance Show Term where
    show (Var _ name)    = name
    show (Abs _ name x)   = "fun " ++ name ++ " -> " ++ show x
    show (App _ x y)     = "(" ++ show x ++ ") (" ++ show y ++ ")"
    show (Let _ name x y) = "let " ++ name ++ " = " ++ show x ++ " in " ++ show y

data Def = Def SourcePos String Term

instance Show Def where
    show (Def _ name x) = "let " ++ name ++ " = " ++ show x


-- de Bruijn indexed terms
data IxTerm =
      IxVar Int
    | IxAbs IxTerm
    | IxApp IxTerm IxTerm
    | IxLet IxTerm IxTerm
    deriving (Eq)

instance Show IxTerm where
    show (IxVar i)   = show i
    show (IxAbs x)   = "fun -> " ++ show x
    show (IxApp x y) = "(" ++ show x ++ ") (" ++ show y ++ ")"
    show (IxLet x y) = "let " ++ show x ++ " in " ++ show y

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

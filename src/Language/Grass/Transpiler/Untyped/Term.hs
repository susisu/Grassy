module Language.Grass.Transpiler.Untyped.Term
    ( Term (..)
    , Def (..)
    , IxTerm (..)
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
    | IxLet IxTerm IxTerm
    | IxApp IxTerm IxTerm

instance Show IxTerm where
    show (IxVar i)   = show i
    show (IxAbs x)   = "fun -> " ++ show x
    show (IxApp x y) = "(" ++ show x ++ ") (" ++ show y ++ ")"
    show (IxLet x y) = "let " ++ show x ++ " in " ++ show y


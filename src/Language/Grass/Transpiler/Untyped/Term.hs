module Language.Grass.Transpiler.Untyped.Term
    ( Pat (..)
    , Term (..)
    , Def (..)
    ) where

import Text.Parsec.Pos (SourcePos)


data Pat = Unbind | Bind String
    deriving (Eq)

instance Show Pat where
    show Unbind      = "_"
    show (Bind name) = name

data Term =
      Var SourcePos String
    | Abs SourcePos Pat Term
    | App SourcePos Term Term
    | Let SourcePos Pat Term Term

instance Show Term where
    show (Var _ name)    = name
    show (Abs _ pat x)   = "fun " ++ show pat ++ " -> " ++ show x
    show (App _ x y)     = "(" ++ show x ++ ") (" ++ show y ++ ")"
    show (Let _ pat x y) = "let " ++ show pat ++ " = " ++ show x ++ " in " ++ show y

data Def = Def SourcePos Pat Term

instance Show Def where
    show (Def _ pat x) = "let " ++ show pat ++ " = " ++ show x

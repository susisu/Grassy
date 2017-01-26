module Language.Grass.Transpiler.Untyped.Term
    ( Pat (..)
    , Term (..)
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
    | Let SourcePos Pat Term Term
    | App SourcePos Term Term

instance Show Term where
    show (Var _ name)    = name
    show (Abs _ pat x)   = "fun " ++ show pat ++ " -> " ++ show x
    show (Let _ pat x y) = "let " ++ show pat ++ " = " ++ show x ++ " in " ++ show y
    show (App _ x y)     = "(" ++ show x ++ ") (" ++ show y ++ ")"

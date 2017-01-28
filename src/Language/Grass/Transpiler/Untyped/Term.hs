module Language.Grass.Transpiler.Untyped.Term
    ( Pattern (..)
    , Term (..)
    , Def (..)
    ) where

import Text.Parsec.Pos (SourcePos)


data Pattern = Unbind | Bind String
    deriving (Eq)

instance Show Pattern where
    show Unbind      = "_"
    show (Bind name) = name

data Term =
      Var SourcePos String
    | Abs SourcePos Pattern Term
    | App SourcePos Term Term
    | Let SourcePos Pattern Term Term

instance Show Term where
    show (Var _ name)    = name
    show (Abs _ pat x)   = "fun " ++ show pat ++ " -> " ++ show x
    show (App _ x y)     = "(" ++ show x ++ ") (" ++ show y ++ ")"
    show (Let _ pat x y) = "let " ++ show pat ++ " = " ++ show x ++ " in " ++ show y

data Def = Def SourcePos Pattern Term

instance Show Def where
    show (Def _ pat x) = "let " ++ show pat ++ " = " ++ show x

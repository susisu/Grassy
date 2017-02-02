{-# LANGUAGE FlexibleContexts #-}

module Language.Grass.Transpiler.Untyped
    ( transpile
    ) where

import Control.Monad.Identity
import qualified Text.Parsec as P

import Language.Grass.Transpiler.Untyped.Term
import Language.Grass.Transpiler.Untyped.Parser
import Language.Grass.Transpiler.Untyped.Transformation

transpile :: P.Stream s Identity Char =>
       String
    -> s
    -> [DefInfo]
    -> (IxTerm -> IxTerm)
    -> CharSet
    -> Either String String
transpile name src ctx opt cs = case parse name src of
    Left err   -> Left ("ParseError at " ++ show err)
    Right defs -> case plant ctx defs opt cs of
        Left err   -> Left err
        Right code -> Right code

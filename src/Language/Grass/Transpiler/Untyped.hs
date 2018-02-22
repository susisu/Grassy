{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Grass.Transpiler.Untyped (
    transpile
  , parse
  , DefInfo (..)
  , Optimizer (..)
  , noOpt
  , Eff (..)
  , elimUnused
  , elimUnused'
  , elimDuplicate
  , elimDuplicate'
  , CharSet (CharSet)
  , defaultCharSet
  , wideCharSet
  , plant
  ) where

import           Control.Monad.Identity
import qualified Text.Parsec            as P

import Language.Grass.Transpiler.Untyped.Optimization
import Language.Grass.Transpiler.Untyped.Parser
import Language.Grass.Transpiler.Untyped.Transformation

transpile :: P.Stream s Identity Char
  => String
  -> s
  -> [DefInfo]
  -> Optimizer
  -> CharSet
  -> Either String String
transpile name src ctx opt cs = case parse name src of
    Left err -> Left ("ParseError at " ++ show err)
    Right defs -> case plant ctx defs opt cs of
      Left err   -> Left err
      Right code -> Right code

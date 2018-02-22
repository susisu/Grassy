{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Grass.Interpreter (
    runGrass
  ) where

import           Control.Monad.Identity
import qualified Text.Parsec            as P

import Language.Grass.Interpreter.Parser
import Language.Grass.Interpreter.VM

runGrass :: P.Stream s Identity Char => String -> s -> IO (Either String ())
runGrass name src =
    case parse name src of
      Left err -> return $ Left ("ParseError at " ++ show err)
      Right code -> do
        res <- run code
        case res of
          Left err -> return $ Left ("RuntimeError at " ++ show err)
          Right () -> return $ Right ()

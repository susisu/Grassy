{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Language.Grass.Interpreter.Parser (parse) where

import           Control.Monad.Identity
import qualified Text.Parsec            as P
import           Text.Parsec.Prim       ((<?>), (<|>))

import Language.Grass.Interpreter.VM

type Parser a = forall s m. P.Stream s m Char
  => P.ParsecT s () m a

whiteSpace :: Parser ()
whiteSpace = P.skipMany (P.noneOf "wWv\xFF57\xFF37\xFF56") <?> ""

headWhiteSpace :: Parser ()
headWhiteSpace = P.skipMany (P.noneOf "w\xFF57") <?> ""

lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

charLowerW :: Parser Char
charLowerW = lexeme (P.oneOf "w\xFF57") <?> "w"

charUpperW :: Parser Char
charUpperW = lexeme (P.oneOf "W\xFF37") <?> "W"

charLowerV :: Parser Char
charLowerV = lexeme (P.oneOf "v\xFF56") <?> "v"

application :: Parser Inst
application = flip P.label "application" $ do
    pos <- P.getPosition
    func <- P.many1 charUpperW
    arg <- P.many1 charLowerW
    return $ App (Pos pos) (length func - 1) (length arg - 1)

abstraction :: Parser Inst
abstraction = flip P.label "abstraction" $ do
    pos <- P.getPosition
    arity <- P.many1 charLowerW
    body <- P.many application
    return $ Abs (Pos pos) (length arity) body

program :: Parser Code
program = do
    headWhiteSpace
    h <- abstraction
    t <- concat <$> P.many (charLowerV >> chunk)
    return $ h : t
  where
    chunk = (return <$> abstraction) <|> P.many application

parse :: P.Stream s Identity Char
  => String -> s -> Either P.ParseError Code
parse = P.parse program

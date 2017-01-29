{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Grass.Transpiler.Untyped.Parser
    ( parse
    ) where

import Control.Monad.Identity
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as TP
import Text.Parsec.Prim ((<|>))

import Language.Grass.Transpiler.Untyped.Term


type Parser a = forall s m. P.Stream s m Char => P.ParsecT s () m a


-- token parsers
langDef :: P.Stream s m Char => TP.GenLanguageDef s u m
langDef = TP.LanguageDef
    { TP.commentStart    = "(*"
    , TP.commentEnd      = "*)"
    , TP.commentLine     = ""
    , TP.nestedComments  = True
    , TP.identStart      = P.letter
    , TP.identLetter     = P.alphaNum <|> P.oneOf "_'"
    , TP.opStart         = TP.opLetter langDef
    , TP.opLetter        = P.oneOf "=->"
    , TP.reservedNames   = ["fun", "let", "in"]
    , TP.reservedOpNames = ["=", "->"]
    , TP.caseSensitive   = True
    }

tp :: P.Stream s m Char => TP.GenTokenParser s u m
tp = TP.makeTokenParser langDef

whiteSpace :: Parser ()
whiteSpace = TP.whiteSpace tp

symbol :: String -> Parser String
symbol = TP.symbol tp

identifier :: Parser String
identifier = TP.identifier tp

reserved :: String -> Parser ()
reserved = TP.reserved tp

reservedOp :: String -> Parser ()
reservedOp = TP.reservedOp tp

parens :: Parser a -> Parser a
parens = TP.parens tp


-- terms
pattern :: Parser String
pattern = identifier <|> (reserved "_" *> pure "_")


term :: Parser Term
term = application <|> abstraction <|> binding <|> parens term

appTerm :: Parser Term
appTerm = variable <|> parens term


variable :: Parser Term
variable = do
    pos  <- P.getPosition
    name <- identifier
    return $ Var pos name

abstraction :: Parser Term
abstraction = flip P.label "abstraction" $ do
    pos <- P.getPosition
    reserved "fun"
    params <- P.many1 pattern
    reservedOp "->"
    body <- term
    return $ foldr (Abs pos) body params

application :: Parser Term
application = flip P.label "application" $ do
    pos  <- P.getPosition
    func <- appTerm
    args <- P.many appTerm
    return $ foldl (App pos) func args

binding :: Parser Term
binding = flip P.label "binding" $ do
    pos <- P.getPosition
    reserved "let"
    name   <- pattern
    params <- P.many pattern
    reservedOp "="
    val <- term
    reserved "in"
    body <- term
    return $ Let pos name (foldr (Abs pos) val params) body


-- definition
definition :: Parser Def
definition = flip P.label "definition" $ do
    pos <- P.getPosition
    reserved "let"
    name   <- pattern
    params <- P.many pattern
    reservedOp "="
    val <- term
    return $ Def pos name (foldr (Abs pos) val params)

separator :: Parser ()
separator = P.skipMany $ symbol ";"

program :: Parser [Def]
program = do
    whiteSpace
    separator
    defs <- P.sepEndBy1 definition separator
    P.eof
    return defs


parse :: P.Stream s Identity Char => String -> s -> Either P.ParseError [Def]
parse name src = P.parse program name src

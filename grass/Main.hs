module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import System.Exit

import Language.Grass.Interpreter (runGrass)


data Options   = Options { optInput :: InputType }
data InputType = Eval String
               | File FilePath

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> version <*> optionsP) $
           fullDesc
        <> header "grass - Grass interpreter"
    where
        version = infoOption "0.1.0.0" $
               short 'v'
            <> long "version"
            <> help "Show the version number"
            <> hidden
        eval = Eval <$> strOption (
               short 'e'
            <> long "eval"
            <> metavar "PROGRAM"
            <> help "Evaluate program"
            )
        file = File <$> strArgument (
               metavar "FILE"
            <> help "Read program from FILE"
            )
        optionsP = Options <$> (eval <|> file)

parseOptions :: IO Options
parseOptions = execParser parserInfo


runProg :: String -> T.Text -> IO ()
runProg name prog = do
    res <- runGrass name prog
    case res of
        Left err -> die err
        Right () -> return ()

main :: IO ()
main = do
    opts <- parseOptions
    case optInput opts of
        Eval prog -> runProg "" (T.pack prog)
        File name -> T.readFile name >>= runProg name

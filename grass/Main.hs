module Main where

import           Data.Monoid
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Options.Applicative
import           System.Exit

import Language.Grass.Interpreter (runGrass)

import Data.Version
import Paths_Grassy (version)


newtype Options = Options { optInput :: InputType }

data InputType =
    FlagInput String
  | FileInput FilePath

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> versionP <*> optionsP)
    $  fullDesc
    <> header "grass - Grass interpreter"
  where
    versionP = infoOption (showVersion version)
        $  short 'v'
        <> long "version"
        <> help "Print version number"
        <> hidden
    evalP = strOption
        $  short 'e'
        <> long "eval"
        <> metavar "PROGRAM"
        <> help "Evaluate one line of program"
    fileP = strArgument
        $  metavar "FILE"
        <> help "Program file"
    optionsP = Options <$> ((FlagInput <$> evalP) <|> (FileInput <$> fileP))

parseOptions :: IO Options
parseOptions = execParser parserInfo


-- run a Grass program
runProg :: String -> T.Text -> IO ()
runProg name prog = do
    res <- runGrass name prog
    case res of
      Left  err -> die err
      Right ()  -> return ()

main :: IO ()
main = do
    opts <- parseOptions
    case optInput opts of
      FlagInput prog -> runProg "" (T.pack prog)
      FileInput name -> T.readFile name >>= runProg name

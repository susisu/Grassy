module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import System.Exit

import Language.Grass.Interpreter (runGrass)


data Options = Options { optVersion :: Bool
                       , optEval    :: (Maybe String)
                       , optFile    :: (Maybe FilePath)
                       }

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsp) $
           fullDesc
        <> header "grass - Grass interpreter"
    where
        versionP = switch $
               short 'v'
            <> long "version"
            <> help "Show the version number"
        evalP = optional . strOption $
               short 'e'
            <> long "eval"
            <> metavar "PROGRAM"
            <> help "Evaluate program"
        fileP = optional . strArgument $
               metavar "FILE"
            <> help "Read program from FILE"
        optionsp = Options <$> versionP
                           <*> evalP
                           <*> fileP

parserPrefs :: ParserPrefs
parserPrefs = defaultPrefs { prefShowHelpOnError = True }

parseOptions :: IO Options
parseOptions = customExecParser parserPrefs parserInfo


showVersion :: IO ()
showVersion = putStrLn "0.0.0.0"

run :: Options -> IO ()
run opts = case optEval opts of
    Just prog -> runProg "" (T.pack prog)
    Nothing   -> case optFile opts of
        Just name -> T.readFile name >>= runProg name
        Nothing   -> return ()

runProg :: String -> T.Text -> IO ()
runProg name prog = do
    res <- runGrass name prog
    case res of
        Left err -> die err
        Right () -> return ()

main :: IO ()
main = do
    opts <- parseOptions
    if optVersion opts
        then showVersion
        else run opts

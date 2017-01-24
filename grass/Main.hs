module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import System.Exit

import Language.Grass.Interpreter (runGrass)


data Options = Options
    Bool             -- version
    (Maybe String)   -- eval
    (Maybe FilePath) -- filename

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
        filenameP = optional . strArgument $ metavar "FILENAME"
        optionsp = Options <$> versionP <*> evalP <*> filenameP

parserPrefs :: ParserPrefs
parserPrefs = defaultPrefs { prefShowHelpOnError = True }

parseOptions :: IO Options
parseOptions = customExecParser parserPrefs parserInfo


showVersion :: IO ()
showVersion = putStrLn "0.0.0.0"

runProg :: String -> T.Text -> IO ()
runProg name prog = do
    res <- runGrass name prog
    case res of
        Left err -> die err
        Right () -> return ()

main :: IO ()
main = parseOptions >>= withOpts
    where
        withOpts (Options True           _           _) = showVersion
        withOpts (Options    _ (Just prog)           _) = runProg "" (T.pack prog)
        withOpts (Options    _           _ (Just name)) = T.readFile name >>= runProg name
        withOpts (Options    _           _           _) = return ()

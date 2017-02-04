module Main where

import qualified Data.Text.IO as T
import Options.Applicative
import System.Exit

import qualified Language.Grass.Transpiler.Untyped as G

data Options = Options { optVersion  :: Bool
                       , optOptimize :: Bool
                       , optWide     :: Bool
                       , optWidth    :: (Maybe Int)
                       , optOutput   :: (Maybe FilePath)
                       , optInput    :: FilePath
                       }

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsp) $
           fullDesc
        <> header "plant - Untyped lambda calculus to Grass transpiler"
    where
        versionP = switch $
               short 'v'
            <> long "version"
            <> help "Show the version number"
        optimizeP = switch $
               short 'O'
            <> long "optimize"
            <> help "Optimize generated code"
        wideP = switch $
               short 'W'
            <> long "wide"
            <> help "Use wide (full-width) characters"
        widthP = optional . option auto $
               short 'w'
            <> long "width"
            <> metavar "INT"
            <> help "Limit maximum number of characters per line to INT"
        outputP = optional . strOption $
               short 'o'
            <> long "output"
            <> metavar "OUTFILE"
            <> help "Write output to OUTFILE"
        inputP = strArgument $
               metavar "INFILE"
            <> help "Read source from INFILE"
        optionsp = Options <$> versionP
                           <*> optimizeP
                           <*> wideP
                           <*> widthP
                           <*> outputP
                           <*> inputP

parserPrefs :: ParserPrefs
parserPrefs = defaultPrefs { prefShowHelpOnError = True }

parseOptions :: IO Options
parseOptions = customExecParser parserPrefs parserInfo


showVersion :: IO ()
showVersion = putStrLn "0.0.0.0"

transpile :: Options -> IO ()
transpile opts = do
    let input = optInput opts
        opt = if optOptimize opts
            then fullOpt
            else G.noOpt
        cs = if optWide opts
            then G.wideCharSet
            else G.defaultCharSet
        format = case optWidth opts of
            Nothing    -> id
            Just width
                | width <= 0 -> id
                | otherwise  -> shape width
        write = case optOutput opts of
            Nothing     -> putStrLn
            Just output -> writeFile output . (++ "\n")
    src <- T.readFile input
    case G.transpile input src ctx opt cs of
        Left err   -> die err
        Right code -> write (format code)
    where
        fullOpt = G.Optimizer { G.globalOpt = G.elimUnused
                              , G.localOpt  = G.elimDuplicate
                              }
        ctx = [ G.DefInfo "Out" 1
              , G.DefInfo "Succ" 1
              , G.DefInfo "w" 1
              , G.DefInfo "In" 1
              ]
        shape _ ""       = ""
        shape width code = case drop width code of
            ""   ->  take width code
            rest -> take width code ++ '\n' : shape width rest

main :: IO ()
main = do
    opts <- parseOptions
    if optVersion opts
        then showVersion
        else transpile opts

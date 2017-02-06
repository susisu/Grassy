module Main where

import qualified Data.Text.IO as T
import Options.Applicative
import System.Exit

import qualified Language.Grass.Transpiler.Untyped as G

data Options = Options { optOptimize :: Bool
                       , optWide     :: Bool
                       , optWidth    :: (Maybe Int)
                       , optOutput   :: (Maybe FilePath)
                       , optInputs   :: [FilePath]
                       }

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> version <*> options) $
           fullDesc
        <> header "plant - Untyped lambda calculus to Grass transpiler"
    where
        version = infoOption "0.1.0.0" $
               short 'v'
            <> long "version"
            <> help "Show the version number"
            <> hidden
        optimize = switch $
               short 'O'
            <> long "optimize"
            <> help "Optimize generated code"
        wide = switch $
               short 'W'
            <> long "wide"
            <> help "Use wide (full-width) characters"
        width = optional . option auto $
               short 'w'
            <> long "width"
            <> metavar "INT"
            <> help "Limit maximum number of characters per line to INT"
        output = optional . strOption $
               short 'o'
            <> long "output"
            <> metavar "OUTFILE"
            <> help "Write output to OUTFILE"
        inputs = some . strArgument $
               metavar "INFILES..."
            <> help "Read sources from INFILES"
        options = Options <$> optimize
                          <*> wide
                          <*> width
                          <*> output
                          <*> inputs

parseOptions :: IO Options
parseOptions = execParser parserInfo


main :: IO ()
main = do
    opts <- parseOptions
    let inputs = optInputs opts
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
    srcs <- mapM T.readFile inputs
    case concat <$> (sequence $ zipWith G.parse inputs srcs) of
        Left err   -> die $ "ParseError at " ++ show err
        Right defs -> case G.plant ctx defs opt cs of
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

module Main where

import           Control.Monad
import           Data.Monoid
import qualified Data.Text.IO        as T
import           Options.Applicative
import           System.Exit

import qualified Language.Grass.Transpiler.Untyped as G

import Data.Version
import Paths_Grassy (version)


data Options = Options {
    optOptimize :: Bool
  , optWide     :: Bool
  , optWidth    :: Maybe Int
  , optOutput   :: Maybe FilePath
  , optInputs   :: [FilePath]
  }

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> versionP <*> optionsP)
    $  fullDesc
    <> header "plant - Untyped lambda calculus to Grass transpiler"
  where
    versionP = infoOption (showVersion version)
        $  short 'v'
        <> long "version"
        <> help "Print version number"
        <> hidden
    optimizeP = switch
        $  short 'O'
        <> long "optimize"
        <> help "Optimize generated code"
    wideP = switch
        $  short 'W'
        <> long "wide"
        <> help "Use wide (full-width) characters"
    widthP = optional . option auto
        $ short 'w'
        <> long "width"
        <> metavar "INT"
        <> help "Specify maximum width of lines"
    outputP = optional . strOption
        $  short 'o'
        <> long "output"
        <> metavar "OUTFILE"
        <> help "Write output to OUTFILE"
    inputsP = some . strArgument
        $  metavar "INFILES..."
        <> help "Source files"
    optionsP = Options
        <$> optimizeP
        <*> wideP
        <*> widthP
        <*> outputP
        <*> inputsP

parseOptions :: IO Options
parseOptions = execParser parserInfo


main :: IO ()
main = do
    opts <- parseOptions
    let
      -- input files
      is = optInputs opts
      -- optimizer
      opt
        | optOptimize opts = fullOpt
        | otherwise = G.noOpt
    -- character set (wide or narrow)
      cs
        | optWide opts = G.wideCharSet
        | otherwise = G.defaultCharSet
      -- formatting function
      format = case optWidth opts of
        Nothing -> id
        Just width
          | width <= 0 -> id
          | otherwise -> split width
      -- output function
      write = case optOutput opts of
        Nothing     -> putStrLn
        Just output -> writeFile output . (++ "\n")
    -- read source files
    srcs <- mapM T.readFile is
    -- parse
    case concat <$> zipWithM G.parse is srcs of
      Left err -> die $ "ParseError at " ++ show err
      Right defs ->
        -- transpile
        case G.plant ctx defs opt cs of
          Left err   -> die err
          Right code -> write (format code)
  where
    fullOpt = G.Optimizer {
        G.globalOpt = G.elimUnused effs
      , G.localOpt = G.elimDuplicate'
      }
    -- context
    ctx = [
        G.DefInfo "Out" 1
      , G.DefInfo "Succ" 1
      , G.DefInfo "w" 1
      , G.DefInfo "In" 1
      ]
    -- side-effects of primitives (used in optimization)
    effs = [G.Eff, G.NoEff, G.NoEff, G.Eff]
    -- split output into lines
    split _ "" = ""
    split width code = case drop width code of
      ""   ->  take width code
      rest -> take width code ++ '\n' : split width rest

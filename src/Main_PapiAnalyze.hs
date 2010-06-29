module Main where

import Analysis
import Control.Monad
import FormulaParser as F
import Formula
import GhcStatsParser
import InputFile
import StatsFile
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Posix.Files

main :: IO ()
main = do
  (config, files) <- parseOpts
  formulas    <- parseFormulas config
  papiResults <- mapM parseFile files
  let rawResults   = collect papiResults
      withFormula  = addFormulaData formulas rawResults
      withSummary  = addSummaryData [] withFormula
      byEvents     = groupByEvents withSummary
  dump withSummary
  dump (addSummaryData [] byEvents)

data Config = Config {
    formulaFile :: Maybe FilePath
  }

defaultConfig :: Config
defaultConfig = Config {
    formulaFile = Just "3.formula"
  }

parseOpts :: IO (Config, [StatsFile])
parseOpts = do
  argv  <- getArgs
  files <- expandFiles argv
  let stats = map checkFileNameFormat files
  return (defaultConfig, stats)

expandFiles :: [FilePath] -> IO [FilePath]
expandFiles files = liftM concat $ mapM expandDir files
  where
  expandDir :: FilePath -> IO [FilePath]
  expandDir f = do
    fs  <- getFileStatus f
    dirs <- if isDirectory fs then getDirectoryContents f else return [f]
    let noDirs = filter (\d -> d /= "." && d /= "..") dirs
    let withParent = if isDirectory fs then (\ff -> f </> ff) else id
    return $ map withParent noDirs

checkFileNameFormat :: FilePath -> StatsFile
checkFileNameFormat file =
  case StatsFile.fromFilePath file of
    Just s  -> s
    Nothing -> 
      error ("Unable to parse stat file name: "++file)

parseFile :: StatsFile -> IO PapiResult 
parseFile statFile = do
  contents <- readFile (toFilePath statFile) >>= (return . lines)
  return $ GhcStatsParser.parse statFile contents

parseFormulas :: Config -> IO [Formula]
parseFormulas (Config {formulaFile = Nothing}) = return []
parseFormulas (Config {formulaFile = Just  f}) = do
  fs <- readFile f
  mapM parseOrDie (InputFile.clean $ lines fs) 
  where
  parseOrDie :: String -> IO Formula
  parseOrDie line =
    case F.parse f line of
      Left err -> do {
          putStr "parse error in formula "; 
          print err; 
          exitFailure } 
      Right x -> return x

{-
PAPI_TOT_CYC BLAH DEE  F1
1000          1000 100
------------------------
AVG mean1 mean2 mean3 geomean

  case getOpt Permute options argv of
    (o,[p,e],[]  ) -> do
      let userConfig = foldl (flip id) defaultConfig o
      let fullConfig = userConfig {optProgramsFile = p, optEventsFile = e}
      config <- expandDirs fullConfig
      checkConfig config
      return config 
    (_,_,errs)  -> do 
      putStrLn (concat errs ++ usageInfo header options)
      exitFailure
  where header = "Usage: papi-collect [OPTION...] programFile eventsFile"
-}

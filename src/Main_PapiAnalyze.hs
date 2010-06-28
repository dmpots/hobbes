module Main where

import Analysis
import Control.Monad
import FormulaParser
import Formula
import GhcStatsParser
import StatsFile
import System.Directory
import System.Environment
import System.FilePath
import System.Posix.Files

main :: IO ()
main = do
  (_config, files) <- parseOpts
  papiResults <- mapM parseFile files
  let rawResults   = collect papiResults
  let finalResults = addSummaryData [] rawResults
  dump finalResults
  --putStrLn (show analysisResults)

data Config = Config 
defaultConfig :: Config
defaultConfig = Config

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

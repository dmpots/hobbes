module Main where

import Analysis
import Control.Monad
import FormulaParser as F
import Formula
import GhcStatsParser
import InputFile
import StatsFile
import System.Console.GetOpt
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
  let sf           = []
      rawResults   = collect papiResults
      withFormula  = addFormulaData formulas rawResults
      withSummary  = addSummaryData sf withFormula
      mergedPs     = (addSummaryData sf . groupProgramsByEvents) withSummary
      mergedEs     = groupEventsByProgram withSummary
      mergedEAndP  = (addSummaryData sf . groupProgramsByEvents) mergedEs
      dumpTarget   = case (optMerge config) of
                        EventsAndPrograms -> mergedEAndP
                        EventsOnly        -> mergedEs
                        ProgramsOnly      -> mergedPs
                        Individual        -> withSummary
  dump dumpTarget

data Merge  = EventsAndPrograms | EventsOnly | ProgramsOnly | Individual
data Config = Config {
      optFormulaFile :: Maybe FilePath
    , optMergeProgs  :: Bool
    , optMergeEvents :: Bool
    , optMerge       :: Merge
  }

defaultConfig :: Config
defaultConfig = Config {
      optFormulaFile = Nothing
    , optMergeProgs  = True
    , optMergeEvents = True
    , optMerge       = EventsAndPrograms
  }

options :: [OptDescr (Config -> Config)]
options =
  [
    Option ['i']     ["individual"]
      (NoArg ((\opts -> opts {
          optMergeProgs = False, optMergeEvents = False
        })))
      "show full counts for each event set"

  , Option ['p']     ["no-merge-progs"]
      (NoArg ((\opts -> opts { optMergeProgs = False })))
      "do not merge programs into single table"

  , Option ['e']     ["no-merge-events"]
      (NoArg ((\opts -> opts { optMergeEvents = False })))
      "do not merge events into single table"

  , Option ['f']     ["formula"]
      (ReqArg ((\f opts -> opts { optFormulaFile = Just f})) "FILE")
      "read formula from file"
  ]



parseOpts :: IO (Config, [StatsFile])
parseOpts = do
  argv  <- getArgs
  case getOpt Permute options argv of
    (o,inputFiles,[]  ) -> do
      files <- expandFiles inputFiles
      let config = setMergeFlag $ foldl (flip id) defaultConfig o
      let stats  = map checkFileNameFormat files
      checkConfig config
      return (config, stats)
    (_,_,errs)  -> do
      putStrLn (concat errs ++ usageInfo header options)
      exitFailure
  where header = "Usage: papi-analyze [OPTION...] FILE [FILE...]"

checkConfig :: Config -> IO ()
checkConfig _ = return ()

setMergeFlag :: Config -> Config
setMergeFlag  c | (optMergeProgs c && optMergeEvents c)
  = c { optMerge = EventsAndPrograms }
setMergeFlag c | ((not . optMergeProgs) c && optMergeEvents c)
  = c { optMerge = EventsOnly }
setMergeFlag c | (optMergeProgs c && (not . optMergeEvents ) c)
  = c { optMerge = ProgramsOnly }
setMergeFlag c | otherwise
  = c { optMerge = Individual}

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
parseFormulas (Config {optFormulaFile = Nothing}) = return []
parseFormulas (Config {optFormulaFile = Just  f}) = do
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


module Main where

import Analysis
import Control.Monad
import FormulaParser as F
import Formula
import GhcStatsParser
import InputFile
import PapiResult
import StatsFile
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Strict as Strict
import XmlStatsParser
import XmlStatsWriter

main :: IO ()
main = do
  (config, statsFiles, xmlFiles) <- parseOpts
  formulas     <- parseFormulas config
  statsResults <- mapM parseGhcResultFile statsFiles
  xmlResults   <- liftM concat $ mapM parseXmlResultFile xmlFiles
  let
      rawResults   = (collect statsResults) ++ (collect xmlResults)
      summarize    = addSummaryData []
      formulize    = addFormulaData formulas
      filterize    = if (optFilterRaw config) then dropRawData else id
      finalResults = (summarize . filterize) dumpTarget
      dumpTarget   =
        case (optMerge config) of
          MergeEventsAndPrograms ->
              ( mergeProgramsForEvents
              . summarize
              . formulize
              . mergeEventsForProgram
              ) rawResults
          MergeEventsOnly        ->
              ( formulize
              . mergeEventsForProgram
              ) rawResults
          MergeProgramsOnly      ->
              ( mergeProgramsForEvents
              . summarize
              . formulize
              ) rawResults
          DoNotMerge             ->
              ( formulize
              ) rawResults
  case optOutputFormat config of
    Tab -> do dumpOnlyPhase "mutator" finalResults
    Xml -> do XmlStatsWriter.printResults finalResults

data Merge  =
  MergeEventsAndPrograms | MergeEventsOnly | MergeProgramsOnly | DoNotMerge

data OutputFormat = Tab | Xml

data Config = Config {
      optFormulaFile :: Maybe FilePath
    , optMergeProgs  :: Bool
    , optMergeEvents :: Bool
    , optMerge       :: Merge
    , optFilterRaw   :: Bool
    , optOutputFormat:: OutputFormat
  }

defaultConfig :: Config
defaultConfig = Config {
      optFormulaFile = Nothing
    , optMergeProgs  = True
    , optMergeEvents = True
    , optMerge       = MergeEventsAndPrograms
    , optFilterRaw   = False
    , optOutputFormat= Tab
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

  , Option ['d']     ["drop-raw"]
      (NoArg ((\opts -> opts { optFilterRaw = True })))
      "drop raw event data"

  , Option ['x']     ["xml-output"]
      (NoArg ((\opts -> opts { optOutputFormat = Xml})))
      "output xml results"
  ]

parseOpts :: IO (Config, [StatsFile], [FilePath])
parseOpts = do
  argv  <- getArgs
  case getOpt Permute options argv of
    (o,inputFiles,[]  ) -> do
      files <- expandFiles inputFiles
      let config = setMergeFlag $ foldl (flip id) defaultConfig o
      let (stats, xmls) = findStatsFiles files
      checkConfig config
      return (config, stats, xmls)
    (_,_,errs)  -> do
      putStrLn (concat errs ++ usageInfo header options)
      exitFailure
  where header = "Usage: papi-analyze [OPTION...] FILE [FILE...]"

checkConfig :: Config -> IO ()
checkConfig _ = return ()

setMergeFlag :: Config -> Config
setMergeFlag  c | (optMergeProgs c && optMergeEvents c)
  = c { optMerge = MergeEventsAndPrograms }
setMergeFlag c | ((not . optMergeProgs) c && optMergeEvents c)
  = c { optMerge = MergeEventsOnly }
setMergeFlag c | (optMergeProgs c && (not . optMergeEvents ) c)
  = c { optMerge = MergeProgramsOnly }
setMergeFlag c | otherwise
  = c { optMerge = DoNotMerge}

expandFiles :: [FilePath] -> IO [FilePath]
expandFiles files = liftM concat $ mapM expandDir files
  where
  expandDir :: FilePath -> IO [FilePath]
  expandDir f = do
    isDir <- doesDirectoryExist f
    dirs <- if isDir then getDirectoryContents f else return [f]
    let noDirs = filter (\d -> d /= "." && d /= "..") dirs
    let withParent = if isDir then (\ff -> f </> ff) else id
    return $ map withParent noDirs

findStatsFiles :: [FilePath] -> ([StatsFile], [FilePath])
findStatsFiles files = go files [] []
  where
  go [] sf xf = (sf, xf)
  go (f:fs) sf xf =
    case StatsFile.fromFilePath f of
      Just s  -> go fs (s:sf)   xf
      Nothing -> go fs    sf (f:xf)


parseGhcResultFile :: StatsFile -> IO (PapiResult EventSetId)
parseGhcResultFile statFile = do
  contents <- Strict.readFile (toFilePath statFile) >>= (return . lines)
  return $ GhcStatsParser.parse statFile contents

parseXmlResultFile :: FilePath -> IO ([PapiResult EventName])
parseXmlResultFile = XmlStatsParser.parseFile

parseFormulas :: Config -> IO [Formula]
parseFormulas (Config {optFormulaFile = Nothing}) = return []
parseFormulas (Config {optFormulaFile = Just  f}) = do
  fs <- Strict.readFile f
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


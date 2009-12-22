module Main where
import Control.Monad
import Cluster
import Data.Char
import Data.Maybe
import GnuPlot
import OpcodeMix
import Opcodes
import PinData
import System.Console.GetOpt
import System.Environment
import System.IO
import System.FilePath.Posix
import System.Random
import Svm

-- Command Line Options
data Options = Options {
    optTitle        :: String
  , optOutPrefix    :: String
  , optNormalize    :: Bool
  , optStacked      :: Bool
  , optThreshold    :: Double
  , optExcelData    :: Bool
  , optNumCluster   :: Maybe Int
  , optWriteGraph   :: Bool
  , optWriteSvm     :: Bool
  , optSvmModel     :: Maybe String
  , optWriteRawData :: Bool
  , optReadRawData  :: Maybe String
}

defaultOptions = Options {
    optTitle        = "PinAlyze"
  , optOutPrefix    = "__PinAlyze__"
  , optNormalize    = True
  , optStacked      = True
  , optThreshold    = 0.0
  , optExcelData    = False
  , optNumCluster   = Nothing
  , optWriteGraph   = False
  , optWriteSvm     = False
  , optSvmModel     = Nothing
  , optWriteRawData = False
  , optReadRawData  = Nothing
}

cmdLineOptions :: [OptDescr (Options -> Options)]
cmdLineOptions = [
      Option ['t'] ["title"]
      (ReqArg (\t opts -> opts { optTitle = t }) "TITLE")
      "Graph Title"

    , Option ['o'] ["output"]
      (ReqArg (\t opts -> opts { optOutPrefix = t }) "PREFIX")
      "Output prefix for .dat and .gnuplot files"

    , Option ['n'] ["normalize counts"]
      (NoArg (\opts -> opts { optNormalize = not (optNormalize opts)}))
      "Normalize counts as a percentage of total"

    , Option ['s'] ["stacked"]
      (NoArg (\opts -> opts { optStacked = not (optStacked opts)}))
      "Generate a stacked histogram"

    , Option ['f'] ["filter"]
      (ReqArg (\t opts -> opts { optThreshold = read t }) "DOUBLE")
      "Filter threshold"

    , Option ['x'] ["excel-data"]
      (NoArg (\opts -> opts { optExcelData = not (optExcelData opts)}))
      "Generate data for excel import"

    , Option ['k'] ["cluster"]
      (ReqArg (\t opts -> opts { optNumCluster = Just(read t) }) "INT")
      "Number of clusters for k-means"

    , Option ['g'] ["write-graph"]
      (NoArg (\opts -> opts { optWriteGraph = not (optWriteGraph opts)}))
      "Write graph and data files"

    , Option ['v'] ["write-svm"]
      (NoArg (\opts -> opts { optWriteSvm = not (optWriteSvm opts)}))
      "Write svm formatted data files"

    , Option ['m'] ["svm-model"]
      (ReqArg (\t opts -> opts { optSvmModel = Just t }) "MODEL_FILE")
      "Use svm model for prediction"

    , Option ['w'] ["write-raw"]
      (NoArg (\opts -> opts { optWriteRawData = not (optWriteRawData opts)}))
      "Write raw formatted data files (using show)"

    , Option ['r'] ["read-raw"]
      (ReqArg (\t opts -> opts { optReadRawData = Just t }) "RAW_FILE")
      "Read raw formatted data files (using read)"
  ] 

main :: IO ()
main = do
    (options, files)<- parseCmdLineOptions =<< getArgs 
    opCodeCounts    <- mapM parseFile files
    filteredResults <- loadAndPrepareResults options opCodeCounts
    processResults options filteredResults

parseCmdLineOptions :: [String] -> IO (Options, [String])
parseCmdLineOptions argv =
  case getOpt Permute cmdLineOptions argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header cmdLineOptions))
  where header = "Usage: pinalyze [OPTION...] files..."


loadAndPrepareResults :: Options->[PinOpCodeData]-> IO [PinOpCodeAnalysisData]
loadAndPrepareResults options results =
  let threshold       = optThreshold options 
      filledResults   = fillMissingData results
      analysisResults = convertToAnalysisData filledResults
      filteredResults = dropUnimportantData threshold analysisResults
      rawDataFile     = optReadRawData options
  in
  if isJust rawDataFile then
    readRawData (fromJust rawDataFile)
  else
    return filteredResults

readRawData :: FilePath -> IO [PinOpCodeAnalysisData]
readRawData fileName = do
  h <- openFile fileName ReadMode
  liftM read (hGetContents h)
  
  

processResults :: Options -> [PinOpCodeAnalysisData] -> IO ()
processResults options []      = return ()
processResults options filteredResults = 
  let graph           = mkGnuPlotGraph info filteredResults
      numClusters     = fromJust (optNumCluster options)
      outPrefix       = optOutPrefix options
      info            = plotInfo options
      clusterElements = convertToClusterElements filteredResults
  in
  do gen <- newStdGen
     writeGnuPlotGraphIf options graph
     writeExcelIf    options graph 
     writeClustersIf options filteredResults
     writeSvmIf options filteredResults
     writeSvmPredictionIf options clusterElements
     writeRawDataIf options filteredResults

plotInfo :: Options -> PlotInfo
plotInfo options = PlotInfo {
          title          = optTitle options
        , scriptFileName = outPrefix ++ ".gnuplot"
        , dataFileName   = outPrefix ++ ".dat"
        , excelFileName  = outPrefix ++ ".txt"
        , normalizeGraph = optNormalize options
        , stackGraph     = optStacked options
    }
    where
    outPrefix       = optOutPrefix options

writeGnuPlotGraphIf :: Options -> GnuPlotGraph -> IO ()
writeGnuPlotGraphIf options graph 
  | optWriteGraph options =
      do putStrLn ("Writing Graph and Data to '" ++(optOutPrefix options)++ "'")
         writeGnuPlotGraph graph
  | otherwise = return ()

writeExcelIf :: Options -> GnuPlotGraph -> IO ()
writeExcelIf options graph
  | optExcelData options = do
       putStrLn ("Writing Excel Data to '" ++ excelFileName ++ "'") 
       writeExcelData graph 
  | otherwise            = return ()
  where
  excelFileName  = (optOutPrefix options) ++ ".txt"

writeClustersIf :: Options -> [PinOpCodeAnalysisData] -> IO ()
writeClustersIf options filteredResults
  | isJust (optNumCluster options) = do
       putStrLn ("Writing Clusters ")
       gen <- getStdGen
       let numClusters  = (fromJust $ optNumCluster options)
       let clusters     = clusterK gen filteredResults numClusters
       writeClusters stdout clusters
  | otherwise            = return ()


writeSvmIf :: Options -> [PinOpCodeAnalysisData] -> IO ()
writeSvmIf options filteredResults
  | optWriteSvm options = do
       putStrLn ("Writing Svm Data")
       h <- openFile fileName WriteMode 
       writeSVMFormattedData h (convertToClusterElements filteredResults)
       hClose h
  | otherwise            = return ()
  where fileName = (optOutPrefix options) ++ ".svm"

writeSvmPredictionIf :: Options -> [OpcodeClusterElement] -> IO ()
writeSvmPredictionIf options filteredResults
  | isJust (optSvmModel options) = do
       let modelFile = fromJust (optSvmModel options)
       putStrLn ("Predicting Svm Data")
       writePredictionDataUsingModel modelFile stdout filteredResults
  | otherwise            = return ()
  where fileName = (optOutPrefix options) ++ ".svm"


writeRawDataIf :: Options -> [PinOpCodeAnalysisData] -> IO ()
writeRawDataIf options filteredResults
  | optWriteRawData options = do
       putStrLn ("Writing Raw Data")
       h <- openFile fileName WriteMode 
       hPutStrLn h (show filteredResults)
       hClose h
  | otherwise            = return ()
  where fileName = (optOutPrefix options) ++ ".raw"

parseFile :: FilePath -> IO PinOpCodeData
parseFile fileName = do
    putStrLn ("Parsing "++fileName)
    h <- openFile fileName ReadMode 
    fileLines <- fmap lines (hGetContents h)
    return $ PinData { 
              bmName   = (formatBmName . takeBaseName) fileName
            , bmLabel  = read (head fileLines)
            , pinData  = map readCount $ tail fileLines
    }
    where

formatBmName :: String -> String    
formatBmName fileName = base ++ rest
  where
  (base, suffix) = splitAtDot fileName
  rest = 
    if all isDigit base  -- add spec number to bench name
    then '.' : (fst $ splitAtDot (dropWhile (== '.') suffix))
    else []
  splitAtDot = span (/= '.')


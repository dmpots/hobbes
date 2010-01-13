module Main where
import Control.Monad
import Cluster
import Data.Char
import Data.Maybe
import GnuPlot
import JumpMix
import OpcodeMix
import PinData
import RegMix
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

defaultOptions :: Options
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
    pinTools        <- mapM parseTool files
    doCheckPinTools pinTools
    case (head pinTools) of
      OpcodeMix -> doOpcodemix options files
      JumpMix   -> doJumpmix   options files
      RegMix    -> doRegmix    options files

doCheckPinTools :: [PinTool] -> IO ()
doCheckPinTools [] = return ()
doCheckPinTools tools = 
  if (all (== (head tools)) tools) then
    return ()
  else
    error "Error: All files must use the same pin tool"

doOpcodemix :: Options -> [FilePath] -> IO ()
doOpcodemix options files = do
  pinCounts <- mapM parseOpcodemixFile files
  doMain options pinCounts OpcodeLabel
                
doJumpmix :: Options -> [FilePath] -> IO ()
doJumpmix options files = do
  pinCounts <- mapM parseJumpmixFile files
  doMain options pinCounts JumpLabel

doRegmix :: Options -> [FilePath] -> IO ()
doRegmix options files = do
  pinCounts <- mapM parseRegmixFile files
  doMain options pinCounts RegLabel

doMain :: Ord k => Options -> [GenCountData k] -> (k -> AnalysisLabel) -> IO ()
doMain options pinCounts mkLabel = do
    filteredResults <- loadAndPrepareResults options pinCounts mkLabel
    processResults options filteredResults

parseCmdLineOptions :: [String] -> IO (Options, [String])
parseCmdLineOptions argv =
  case getOpt Permute cmdLineOptions argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header cmdLineOptions))
  where header = "Usage: pinalyze [OPTION...] files..."


loadAndPrepareResults :: Ord k 
  => Options 
  -> [GenCountData k]
  -> (k -> AnalysisLabel)
  -> IO [PinAnalysisData]
loadAndPrepareResults options results mkLabel =
  let threshold       = optThreshold options 
      filledResults   = fillMissingData results
      analysisResults = convertToAnalysisData filledResults mkLabel
      filteredResults = dropUnimportantData threshold analysisResults
      rawDataFile     = optReadRawData options
  in
  if isJust rawDataFile then
    readRawData (fromJust rawDataFile)
  else
    return filteredResults

readRawData :: FilePath -> IO [PinAnalysisData]
readRawData fileName = do
  h <- openFile fileName ReadMode
  liftM read (hGetContents h)
  
processResults :: Options -> [PinAnalysisData] -> IO ()
processResults _       []              = return ()
processResults options filteredResults = 
  let graph           = mkGnuPlotGraph info filteredResults
      info            = plotInfo options
      clusterElements = convertToClusterElements filteredResults
  in
  do 
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
  | optExcelData options = writeExcelData graph 
  | otherwise            = return ()

writeClustersIf :: Options -> [PinAnalysisData] -> IO ()
writeClustersIf options filteredResults
  | isJust (optNumCluster options) = do
       putStrLn ("Writing Clusters ")
       gen <- getStdGen
       let numClusters  = (fromJust $ optNumCluster options)
       let clusters     = clusterK gen filteredResults numClusters
       writeClusters stdout clusters filteredResults
  | otherwise            = return ()


writeSvmIf :: Options -> [PinAnalysisData] -> IO ()
writeSvmIf options filteredResults
  | optWriteSvm options = do
       putStrLn ("Writing Svm Data")
       h <- openFile fileName WriteMode 
       writeSVMFormattedData h (convertToClusterElements filteredResults)
       hClose h
  | otherwise            = return ()
  where fileName = (optOutPrefix options) ++ ".svm"

writeSvmPredictionIf :: Options -> [PinClusterElement] -> IO ()
writeSvmPredictionIf options filteredResults
  | isJust (optSvmModel options) = do
       let modelFile = fromJust (optSvmModel options)
       putStrLn ("Predicting Svm Data")
       writePredictionDataUsingModel modelFile stdout filteredResults
  | otherwise            = return ()


writeRawDataIf :: Options -> [PinAnalysisData] -> IO ()
writeRawDataIf options filteredResults
  | optWriteRawData options = do
       putStrLn ("Writing Raw Data")
       h <- openFile fileName WriteMode 
       hPutStrLn h (show filteredResults)
       hClose h
  | otherwise            = return ()
  where fileName = (optOutPrefix options) ++ ".raw"

parseTool :: FilePath -> IO PinTool
parseTool fileName = do
    h <- openFile fileName ReadMode 
    toolName <- hGetLine h
    hClose h
    return $ read toolName

parseOpcodemixFile :: FilePath -> IO PinOpcodeData
parseOpcodemixFile fileName = parseFile fileName readOpcodeCount

parseJumpmixFile :: FilePath -> IO PinJumpData
parseJumpmixFile fileName = parseFile fileName readJumpCount

parseRegmixFile :: FilePath -> IO PinRegData
parseRegmixFile fileName = parseFile fileName readRegCount

parseFile :: FilePath -> (String -> (k, PinCounter)) -> IO (GenCountData k)
parseFile fileName reader = do
    putStrLn ("Parsing "++fileName)
    h <- openFile fileName ReadMode 
    fileLines <- readFileLines h
    hClose h
    let [_,progClass]  = take 2 fileLines
    let body           = drop 2 fileLines
    return $ PinData { 
              bmName   = (formatBmName . takeBaseName) fileName
            , bmLabel  = read progClass
            , pinData  = map reader body
    }

readFileLines :: Handle -> IO [String]
readFileLines h = do
  do { eof <- hIsEOF h
     ; if eof then 
        return []
       else do { line <- hGetLine h
               ; rest <- readFileLines h
               ; return (line : rest) }
     }

formatBmName :: String -> String    
formatBmName fileName = base ++ rest
  where
  (base, suffix) = splitAtDot fileName
  rest = 
    if all isDigit base  -- add spec number to bench name
    then '.' : (fst $ splitAtDot (dropWhile (== '.') suffix))
    else []
  splitAtDot = span (/= '.')


module Main where
import Control.Monad
import Data.Char
import GnuPlot
import OpcodeMix
import Opcodes
import System.Console.GetOpt
import System.Environment
import System.IO
import System.FilePath.Posix


-- Command Line Options
data Options = Options {
    optTitle      :: String
  , optOutPrefix  :: String
  , optNormalize  :: Bool
  , optStacked    :: Bool
  , optThreshold  :: Double
  , optExcelData  :: Bool
}

defaultOptions = Options {
    optTitle     = "PinAlyze"
  , optOutPrefix = "__PinAlyze__"
  , optNormalize = True
  , optStacked   = True
  , optThreshold = 0.0
  , optExcelData = False
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
      "Normalize counts as a percentage of total for each benchmark"

    , Option ['s'] ["stacked"]
      (NoArg (\opts -> opts { optStacked = not (optStacked opts)}))
      "Generate a stacked histogram"

    , Option ['f'] ["filter"]
      (ReqArg (\t opts -> opts { optThreshold = read t }) "DOUBLE")
      "Filter threshold"

    , Option ['x'] ["excel-data"]
      (NoArg (\opts -> opts { optExcelData = not (optExcelData opts)}))
      "Generate data for excel import"
  ] 

main :: IO ()
main = do
    (options, files) <- parseCmdLineOptions =<< getArgs 
    opCodeCounts <- mapM parseFile files
    createGraph options opCodeCounts

parseCmdLineOptions :: [String] -> IO (Options, [String])
parseCmdLineOptions argv =
  case getOpt Permute cmdLineOptions argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header cmdLineOptions))
  where header = "Usage: pinalyze [OPTION...] files..."


createGraph :: Options -> [PinOpCodeData] -> IO ()
createGraph options []      = return ()
createGraph options results = do
    let info = PlotInfo {
          title          = optTitle options
        , scriptFileName = outPrefix ++ ".gnuplot"
        , dataFileName   = outPrefix ++ ".dat"
        , excelFileName  = outPrefix ++ ".txt"
        , normalizeGraph = optNormalize options
        , stackGraph     = optStacked options
    }
        threshold       = optThreshold options 
        filledResults   = fillMissingData results
        analysisResults = convertToAnalysisData filledResults
        filteredResults = dropUnimportantData threshold analysisResults
        graph           = mkGnuPlotGraph info filteredResults
        outPrefix       = optOutPrefix options

    -- Only output data file here if desired
    putStrLn ("Writing Graph and Data to '" ++ outPrefix ++ "'")
    writeGnuPlotGraph graph
    if optExcelData options then  
       writeExcelData graph >> 
       putStrLn ("Writing Excel Data to '" ++ (excelFileName info) ++ "'") 
      else return ()

parseFile :: FilePath -> IO PinOpCodeData
parseFile fileName = do
    putStrLn ("Parsing "++fileName)
    h <- openFile fileName ReadMode 
    fileLines <- fmap lines (hGetContents h)
    return $ OpData { 
              bmName   = (formatBmName . takeBaseName) fileName
            , opCounts = map readCount fileLines
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


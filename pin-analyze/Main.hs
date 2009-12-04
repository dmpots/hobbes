module Main where
import Control.Monad
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
}

defaultOptions = Options {
    optTitle     = "PinAlyze"
  , optOutPrefix = "__PinAlyze__"
  , optNormalize = True
  , optStacked   = True
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
        , normalizeGraph = optNormalize options
        , stackGraph     = optStacked options
    }
        filledResults = fillMissingData results
    putStrLn ("Writing Graph and Data to '" ++ outPrefix ++ "'")
    (writeGnuPlotGraph . (mkGnuPlotGraph info)) filledResults

    where outPrefix = optOutPrefix options


parseFile :: FilePath -> IO PinOpCodeData
parseFile fileName = do
    putStrLn ("Parsing "++fileName)
    h <- openFile fileName ReadMode 
    fileLines <- fmap lines (hGetContents h)
    return $ OpData { 
              bmName   = takeBaseName fileName
            , opCounts = map readCount fileLines
    }
    where
    readCount = dropFirst . (read :: String -> (OpId, Opcodes.Opcode, OpCount))
    dropFirst (x,y,z) = (y,z)


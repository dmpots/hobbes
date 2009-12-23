module GnuPlot where
import PinData
import Data.List
import Numeric
import System.IO
import System.FilePath.Posix
import Util

data PlotInfo = PlotInfo {
      title          :: String
    , dataFileName   :: FilePath
    , scriptFileName :: FilePath
    , excelFileName  :: FilePath
    , normalizeGraph :: Bool
    , stackGraph     :: Bool
}

data DataColumn = I Integer | S String | F Double
    deriving(Show, Eq)

type GraphScript  = String
type GraphData    = [[DataColumn]]
type GnuPlotGraph = (PlotInfo, GraphScript, GraphData)
type Title = String
type GraphHeader = String

mkGnuPlotGraph :: PlotInfo -> [PinAnalysisData] -> GnuPlotGraph
mkGnuPlotGraph info counts = (info, graphScript, graphData)
    where
        graphScript = mkGraphHeader info counts
        graphData   = mkGraphData counts



mkGraphHeader :: PlotInfo -> [GenPinData a] -> GraphHeader
mkGraphHeader info counts = concat $ intersperse "\n" (plotHeader info counts)
plotHeader :: PlotInfo -> [a] -> [String]
plotHeader info counts = [
      "BASE_FILENAME='"++(takeBaseName.dataFileName $ info)++"'"
    , "OUT_FILENAME=BASE_FILENAME.'.eps'"
    , "DAT_FILENAME=BASE_FILENAME.'.dat'"
    , terminal
    , "set output OUT_FILENAME"
    , "set title'"++(title info) ++"'"
    , xtic
    , "set key autotitle columnheader"
    , "set key invert reverse Left outside"
    , "set boxwidth 0.9 relative"
    , "set style fill solid border lt -1"
    , "set style data histogram"
    , histoStyle
    , "set style fill solid"
    , plotCommand
    ] 
    where
    -- select actual counts or normalized data based on user preference
    s = if normalizeGraph info then 3 else 2
    e = (s + 2 * ((length counts) - 1))
    range = [s,(s+2)..e]
    plotCommand = 
        "plot DAT_FILENAME using "++(show s)++":"++(keyOrXtic)
        ++ " title column "
        ++(concatMap (\c -> "\\\n    ,DAT_FILENAME using " ++(show c)) 
          (tail range))

    -- select terminal based on user preference
    terminal   = "set terminal x11"   ++ "\n" ++
                 "#set terminal aqua" ++ "\n" ++
                 "#set terminal postscript eps color colortext solid enhanced"

    -- different settings for stacked or plain histograms
    xtic       = if stacked then "set xtic rotate by -90"
                            else "set xtic rotate by -90"
    histoStyle = if stacked then "set style histogram columnstacked"
                            else "set style histogram clustered gap 2"
    keyOrXtic  = if stacked then "key(1)" else "xtic(1)"
    stacked    = (stackGraph info)

mkGraphData :: [PinAnalysisData] -> GraphData
mkGraphData []     = [[]]
mkGraphData counts = 
    let -- colLabels like File1.log, File2.log
        colLabels   = generateColumnLabels counts
        -- rowLabels like ADD, MOV, etc.
        rowLabels   = map (S . formatOpLabels . label) (pinData $ head  counts)
        -- columns are counts of operations
        countCols   = map formatCnts rawCounts
        -- percentColumns are percent of total ops
        percentCols = map formatPercents rawPercents
        -- data access utilites
        formatCnts     = map I 
        formatPercents = map F
        analysisData   = map pinData counts :: [[AnalysisData]]
        rawCounts      = map (map rawCount)     analysisData
        rawPercents    = map (map percentTotal) analysisData
    in
    colLabels : transpose (rowLabels : (weave countCols percentCols))

formatOpLabels :: AnalysisLabel -> String
formatOpLabels (OpcodeLabel o) = show o
formatOpLabels (JumpLabel  jl) = show jl

generateColumnLabels :: [GenPinData a] -> [DataColumn]
generateColumnLabels counts = opcodeLabel ++ columnLabels
    where
    opcodeLabel   = [S "opcode"]
    bmLabels      = map (S . bmName) counts
    percentLabels = map (S . (++" %") . bmName) counts
    columnLabels  = weave bmLabels percentLabels

writeGnuPlotGraph :: GnuPlotGraph -> IO ()
writeGnuPlotGraph graph = 
    let (info, script, dataFile) = graph in
    do
        writeGraphScript info script
        writeGraphData info dataFile

writeGraphScript :: PlotInfo -> GraphScript -> IO ()
writeGraphScript info script = do
      h <- openFile (scriptFileName info) WriteMode
      hPutStrLn h script
      hClose h

writeGraphData :: PlotInfo -> GraphData -> IO ()
writeGraphData info fileData = do
      h <- openFile (dataFileName info) WriteMode
      mapM_ (hPutStrLn h . formatDataRow) fileData
      hClose h

formatDataRow :: [DataColumn] -> String
formatDataRow dataRow = format dataRow 
    where
    format = concat . (intersperse "\t") . (map extract)
    extract (I i) = show i
    extract (S s) = show s
    extract (F f) = (showFFloat (Just 4) f) ""
     
writeExcelData :: GnuPlotGraph -> IO ()
writeExcelData graph = do
  let (info, _, dataFile) = graph 
      selectFun = if (normalizeGraph info) then oddPositions 
        else (\l -> (head l) : evenPositions l)
  putStrLn ("Writing Excel Data to '" ++ (excelFileName info) ++ "'") 
  h <- openFile (excelFileName info) WriteMode
  mapM_ (hPutStrLn h . formatDataRow . selectFun) dataFile 
  hClose h


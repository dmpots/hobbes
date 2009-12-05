module GnuPlot where
import OpcodeMix
import Data.List
import Numeric
import System.IO
import System.FilePath.Posix

data PlotInfo = PlotInfo {
      title          :: String
    , dataFileName   :: FilePath
    , scriptFileName :: FilePath
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

mkGnuPlotGraph :: PlotInfo -> [PinOpCodeData] -> GnuPlotGraph
mkGnuPlotGraph info counts = (info, graphScript, graphData)
    where
        graphScript = mkGraphHeader info counts
        graphData   = mkGraphData info counts



mkGraphHeader :: PlotInfo -> [PinOpCodeData] -> GraphHeader
mkGraphHeader info counts = concat $ intersperse "\n" (header info counts)
header info counts = [
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
    , "plot for [COL="++s++":"++e++":2] DAT_FILENAME using COL:"++(keyOrXtic)
      ++ " title column"
    ] 
    where
    -- select actual counts or normalized data based on user preference
    s = show (if normalizeGraph info then 3 else 2)
    e = show ((read s) + 2 * ((length counts) - 1))

    -- select terminal based on user preference
    terminal   = "set terminal x11"   ++ "\n" ++
                 "#set terminal aqua" ++ "\n" ++
                 "#set terminal postscript eps color colortext solid enhanced"

    -- different settings for stacked or plain histograms
    xtic       = if stacked then "set xtic rotate by   0"
                            else "set xtic rotate by -90"
    histoStyle = if stacked then "set style histogram columnstacked"
                            else "set style histogram clustered gap 2"
    keyOrXtic  = if stacked then "key(1)" else "xtic(1)"
    stacked    = (stackGraph info)

{-
#set style histogram columnstacked
#plot for [COL=2:2] DAT_FILENAME using COL:key(1) 
-}

mkGraphData :: PlotInfo -> [PinOpCodeData] -> GraphData
mkGraphData info []     = [[]]
mkGraphData info counts = 
    let -- colLabels like File1.log, File2.log
        colLabels   = generateColumnLabels counts
        -- rowLabels like ADD, MOV, etc.
        rowLabels   = map (S . show . fst) (opCounts . head $ counts)
        -- columns are counts of operations
        countCols   = map formatCnts rawCounts
        -- percentColumns are percent of total ops
        percentCols = map percentsOfTotal rawCounts
        -- data access utilites
        formatCnts  = map I 
        bmOpCounts  = map opCounts counts
        rawCounts   = map (map snd) bmOpCounts
    in
    colLabels : transpose (rowLabels : (weave countCols percentCols))

generateColumnLabels :: [PinOpCodeData] -> [DataColumn]
generateColumnLabels counts = opcodeLabel ++ columnLabels
    where
    opcodeLabel   = [S "opcode"]
    bmLabels      = map (S . bmName) counts
    percentLabels = map (S . (++" - % of Total") . bmName) counts
    columnLabels  = weave bmLabels percentLabels


percentsOfTotal :: [Integer] -> [DataColumn]
percentsOfTotal counts = map (F.(/total).fromIntegral) counts
    where total = fromIntegral (sum counts)

weave :: [a] -> [a] -> [a]
weave a b = concat $ zipWith (\x y -> x:y:[]) a b

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
     

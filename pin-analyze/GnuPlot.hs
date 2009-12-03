module GnuPlot where
import OpcodeMix
import Data.List
import System.IO
import System.FilePath.Posix

data PlotInfo = PlotInfo {
      title          :: String
    , dataFileName   :: FilePath
    , scriptFileName :: FilePath
}

data DataColumn = I Integer | S String
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
    , "set terminal postscript eps"
    , "set output OUT_FILENAME"
    , "set title'"++(title info) ++"'"
    , "set xtic rotate by -90"
    , "set key autotitle columnheader"
    , "set key invert reverse Left outside"
    , "set style data histogram"
    , "set style fill solid"
    , "plot for [COL=2:"++c++"] DAT_FILENAME using COL:xtic(1) title column"
    ] 
    where
    c = show (1 + (length counts))
{-
#set style histogram columnstacked
#plot for [COL=2:2] DAT_FILENAME using COL:key(1) 
-}

mkGraphData :: PlotInfo -> [PinOpCodeData] -> GraphData
mkGraphData info []     = [[]]
mkGraphData info counts = 
    let colLabels   = [S "opcode"] ++ map (S . bmName) counts
        rowLabels   = map (S . show . fst) (opCounts . head $ counts)
        columns     = map  extractCnts (map opCounts counts)
        extractCnts = map (I . snd) 
    in
    colLabels : transpose (rowLabels : columns)

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
    format = concat . (intersperse " ") . (map extract)
    extract (I i) = show i
    extract (S s) = show s
     

    
    

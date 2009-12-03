module Main where
import Control.Monad
import GnuPlot
import OpcodeMix
import Opcodes
import System.IO
import System.Environment
import System.FilePath.Posix

main :: IO ()
main = do
    args <- getArgs
    opCodeCounts <- mapM parseFile args
    createGraph args opCodeCounts

createGraph :: [String] -> [PinOpCodeData] -> IO ()
createGraph args []      = return ()
createGraph args results = do
    let fileName = head args
    let info = PlotInfo {
          title          = ""
        , scriptFileName = takeBaseName fileName ++ ".gnuplot"
        , dataFileName   = takeBaseName fileName ++ ".dat"
    }
    putStrLn ("Creating Graphs")
    (writeGnuPlotGraph . (mkGnuPlotGraph info)) results


parseFile :: FilePath -> IO PinOpCodeData
parseFile fileName = do
    putStrLn ("Parsing "++fileName)
    h <- openFile fileName ReadMode 
    fileLines <- fmap lines (hGetContents h)
    return $ OpData { 
              bmName =fileName
            , opCounts = map readCount fileLines
    }
    where
    readCount = dropFirst . (read :: String -> (OpId, Opcodes.Opcode, OpCount))
    dropFirst (x,y,z) = (y,z)


module Main where
import Control.Monad
import Opcodes
import System.IO
import System.Environment

type OpId    = Int
type OpCount = Integer
type Counts  = (OpId, Opcode, OpCount)

main :: IO ()
main = do
    args <- getArgs
    mapM_ processFile args

processFile :: FilePath -> IO ()
processFile fileName = do
    putStrLn ("Processing "++fileName)
    results <- parseFile fileName
    mapM_ (putStrLn.show) results

parseFile :: FilePath -> IO [Counts]
parseFile fileName = do
    h <- openFile fileName ReadMode 
    fileContents <- hGetContents h
    let fileLines = lines fileContents
    return $ map read fileLines
    



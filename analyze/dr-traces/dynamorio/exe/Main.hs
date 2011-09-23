module Main where

import Data.Elf.Symtab
import Data.Word
import Control.Monad
import Text.Parsec.String
import Numeric
import qualified Data.ByteString.Char8 as B
import DynamoRIO.Trace(Trace(..))
import DynamoRIO.TraceDumpParser(parser)
import System.Environment
import System.Exit


main = do
  (exe, trace_txt) <- parseArgs
  Just symtab <- readSymbolTables exe
  result <- parseFromFile parser trace_txt
  case result of
    Left err -> print err
    Right ts -> mapM_ (dumpTrace symtab) ts

parseArgs :: IO (FilePath, FilePath)
parseArgs = do
  args <- getArgs
  case args of
    (exe:trace:[]) -> return (exe,trace)
    _              -> putStrLn "usage: label [exe] [tracefile]" >> exitFailure


dumpTrace :: SymbolTable -> Trace -> IO ()
dumpTrace symtab (Trace n blocks) = do
  putStrLn $ "Trace " ++ (show n)
  mapM_ (\b -> putStrLn("    " ++ sym symtab b)) blocks
  where 
  sym t a = 
    case steName `liftM` findSymbolByAddress t a of
      Just (_, Just name)  -> B.unpack name
      _                    -> "0x" ++ showHex a ""


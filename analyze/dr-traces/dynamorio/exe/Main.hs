module Main where

import Data.Elf.Symtab
import Data.Word
import Data.Maybe(mapMaybe)
import qualified Data.Map as M
import Control.Monad
import Text.Parsec.String
import Numeric
import qualified Data.ByteString.Char8 as B
import DynamoRIO.Trace
import DynamoRIO.TraceDumpParser(parser)
import System.Environment
import System.Exit
import qualified Language.Dot as Dot


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
    (command:exe:trace:profile:[]) -> return (exe,trace)
    _ -> putStrLn ("usage: label <command> <exe> <tracefile> <profile>" ++
                   "\n  command is [dot|bb|prof]")
         >> exitFailure


dumpTrace :: SymbolTable -> Trace -> IO ()
dumpTrace symtab (Trace n tag blocks _) = do
  putStrLn $ "Trace " ++ (show n)
  mapM_ (\b -> putStrLn("    " ++ sym symtab b)) blocks
  where 
  sym t a = 
    case steName `liftM` findSymbolByAddress t a of
      Just (_, Just name)  -> B.unpack name
      _                    -> "0x" ++ showHex a ""

type TraceMap = M.Map CacheAddr (Trace)
buildTraceMap :: [Trace] -> M.Map CacheAddr (Trace)
buildTraceMap = foldr f M.empty
  where f trace m = foldr (\entry m' -> M.insert (entryAddr entry) trace m') m (entries trace)
          where entries = traceEntries . traceLayout


buildDotGraph :: [Trace] -> Dot.Graph
buildDotGraph traces = mkGraph (nodes ++ edges)
  where 
    mkGraph = Dot.Graph Dot.StrictGraph Dot.DirectedGraph Nothing
    nodes   = map (mkNode . traceId) traces
    edges   = concatMap addEdges traces
    mkNode  = (flip Dot.NodeStatement [] . mkId)
    mkId    = mkId_s . show
    mkId_s  = flip Dot.NodeId Nothing . Dot.NameId
    mkEdge srcT sink = Dot.EdgeStatement ent att
      where ent = [Dot.ENodeId Dot.NoEdge (mkId  (traceId srcT)),
                   Dot.ENodeId Dot.DirectedEdge  (mkId_s sink)]
            att = [Dot.AttributeSetValue (Dot.NameId "style") (Dot.NameId "dotted")]
    addEdges trace = mapMaybe f ((traceExits . traceLayout) trace)
      where 
        f (ExitLinked addr) = Just (mkEdge trace sink)
          where sink = case M.lookup addr traceMap of
                                Just t  -> show (traceId t)
                                Nothing -> "_should_be_linked"
        f _                 = Nothing
    traceMap = buildTraceMap traces
    


test = do
  result <- parseFromFile parser "_Trace.txt"
  case result of
    Left err -> print err
    Right ts -> putStrLn $ Dot.renderDot (buildDotGraph ts)
  

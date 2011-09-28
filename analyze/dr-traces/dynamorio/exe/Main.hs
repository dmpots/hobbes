module Main(main) where

import Data.Elf.Symtab
import Data.Word
import Data.Maybe(mapMaybe)
import Data.List(foldl', sortBy)
import Data.Function(on)
import qualified Data.Map as M
import Control.Monad
import Text.Parsec.String
import Numeric
import qualified Data.ByteString.Char8 as B
import DynamoRIO.Trace
import DynamoRIO.TraceDumpParser as T
import DynamoRIO.PcSampleParser  as S
import DynamoRIO.PcSample(PcSample(..), SampleCount, SampleLoc(..))
import System.Environment
import System.Exit
import Text.Printf(printf)
import qualified Language.Dot as Dot


-------------------------------------------------------------------------------
-- Main Entry
-------------------------------------------------------------------------------
main = do
  (command, exe, trace_txt, prof_txt) <- parseArgs
  symtab  <- readSymbolTables exe             >>= checkSyms
  traces  <- parseFromFile T.parser trace_txt >>= checkParse
  samples <- parseFromFile S.parser prof_txt  >>= checkParse
  let traceMap  = buildTraceMap traces
      sampleMap = buildSampleCount samples traceMap
  case command of
    "bb"  -> mapM_ (dumpTrace symtab) traces
    "dot" -> printDot traces noStyle
    "prof"-> printDot traces (hotStyleNode sampleMap)
    "prof-table"-> putStrLn (profTable sampleMap)
    _     -> putStrLn ("Unknown command: "++command) >> exitFailure
  where 
    checkParse (Left  err) = print err >> exitFailure
    checkParse (Right r)   = return r
    checkSyms  (Just  s)   = return s
    checkSyms   Nothing    = print "Error reading Elf symbtab" >> exitFailure
    printDot traces style  = putStrLn (Dot.renderDot (buildDotGraph style traces))

parseArgs :: IO (String, FilePath, FilePath, FilePath)
parseArgs = do
  args <- getArgs
  case args of
    (command:exe:trace:profile:[]) -> return (command, exe,trace,profile)
    _ -> putStrLn ("usage: label <command> <exe> <tracefile> <profile>" ++
                   "\n  command is [dot|bb|prof]")
         >> exitFailure

-------------------------------------------------------------------------------
-- Traces
-------------------------------------------------------------------------------
dumpTrace :: SymbolTable -> Trace -> IO ()
dumpTrace symtab (Trace n tag blocks _) = do
  putStrLn $ "Trace " ++ (show n)
  mapM_ (\b -> putStrLn("    " ++ sym symtab b)) blocks
  where 
  sym t a = 
    case steName `liftM` findSymbolByAddress t a of
      Just (_, Just name)  -> B.unpack name
      _                    -> "0x" ++ showHex a ""

type TraceMap = M.Map CacheAddr Trace
buildTraceMap :: [Trace] -> TraceMap
buildTraceMap = foldr f M.empty
  where f trace m = foldr (\entry m' -> M.insert (entryAddr entry) trace m') m (entries trace)
          where entries = traceEntries . traceLayout

-------------------------------------------------------------------------------
-- Dot Graph
-------------------------------------------------------------------------------
buildDotGraph :: NodeStyle -> [Trace] -> Dot.Graph
buildDotGraph style traces = mkGraph (nodes ++ edges)
  where 
    mkGraph = Dot.Graph Dot.StrictGraph Dot.DirectedGraph Nothing
    nodes   = map (mkNode . traceId) traces
    edges   = concatMap addEdges traces
    mkNode t= (flip Dot.NodeStatement (style t) . mkId) t
    mkId    = mkId_s . show
    mkId_s  = flip Dot.NodeId Nothing . Dot.NameId
    mkEdge srcT sink = Dot.EdgeStatement ent att
      where ent = [Dot.ENodeId Dot.NoEdge (mkId  (traceId srcT)),
                   Dot.ENodeId Dot.DirectedEdge  (mkId_s sink)]
            att = []
    addEdges trace = mapMaybe f ((traceExits . traceLayout) trace)
      where 
        f (ExitLinked addr) = Just (mkEdge trace sink)
          where sink = case M.lookup addr traceMap of
                                Just t  -> show (traceId t)
                                Nothing -> "_should_be_linked"
        f _                 = Nothing
    traceMap = buildTraceMap traces

-------------------------------------------------------------------------------
-- PC Samples
-------------------------------------------------------------------------------
type SampleMap = M.Map TraceId SampleCount
buildSampleCount :: [PcSample] -> TraceMap -> SampleMap
buildSampleCount samples traceMap = buildMap traceSamples
  where
  buildMap     = foldl' (\m s -> insertEntry (sampleAddr s) (sampleCount s) m) M.empty
  insertEntry  a c m = 
    case M.lookup a traceMap of
      Just t  -> M.insertWith' (+) (traceId t) c m
      Nothing -> m
  traceSamples = filter (isTraceSample . sampleLoc) samples
  isTraceSample (TraceLoc _) = True
  isTraceSample ____________ = False

profTable :: SampleMap -> String
profTable sampleMap = unlines rows
  where
  rows     = map (uncurry tableRow) (samplesSortedByCount sampleMap)
  tableRow tid cnt = printf "Trace %4d %10d %10.2f%%" tid cnt (100 * (d cnt / d total))
  total = totalMap sampleMap

samplesSortedByCount :: SampleMap -> [(TraceId, SampleCount)]
samplesSortedByCount sampleMap = (sortBy (flip compare `on` snd) (M.toAscList sampleMap))

d :: Integral a => a -> Double
d = fromIntegral

totalMap :: Num v => M.Map k v -> v
totalMap = M.fold (\s c -> let x = s + c in x `seq` x) 0

-------------------------------------------------------------------------------
-- Dot Styles
-------------------------------------------------------------------------------
type NodeStyle = (TraceId -> [Dot.Attribute])
hotStyleNode :: SampleMap -> NodeStyle
hotStyleNode sampleMap = style
  where 
  total = totalMap sampleMap
  style t = 
    case M.lookup t sampleMap of
      Nothing  -> []
      Just cnt -> 
        let color = chooseColor cnt in
        [Dot.AttributeSetValue (Dot.NameId "style") (Dot.NameId "filled"),
         Dot.AttributeSetValue (Dot.NameId "fillcolor") color]
  colors = map Dot.StringId ["#1000ab", "#2db000","#8fca00","#fdff00","#fac700","#f99400","#f70000"]
  chooseColor = chooseRelative
  chooseAbsolute cnt =
    let index = floor ((d cnt / d total) * numColors) in (colors !! index)
  chooseRelative cnt = 
    if cnt < lub then cool else maybe cool id (lookup cnt top)
  top = zip (map snd (samplesSortedByCount sampleMap)) (reverse colors)
  cool = head colors
  lub = fst (last top)
  numColors = d (length colors)

noStyle :: NodeStyle
noStyle = const []

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

test = do
  result <- parseFromFile S.parser "_Profile.txt"
  case result of
    Left err -> print err
    Right ps -> putStrLn $ show(ps) --
  

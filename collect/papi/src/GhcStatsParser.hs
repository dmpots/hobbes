module GhcStatsParser (
    parse
)
where
import Data.Char
import Data.List
import qualified Data.Set as Set
import PapiResult
import PhaseData
import StatsFile

parse :: StatsFile -> [String] -> PapiResult EventSetId
parse sf stats = 
  PapiResult {
      programName  = progName sf
    , papiEvents   = Set.singleton (eventSetId sf)
    , phaseResults =
          addPhase "mutator" mutatorLines $
          addPhase "gc0" gc0Lines $
          addPhase "gc1" gc1Lines $
          emptyPhaseData
  }
  where
  mutatorLines = map parseLine $ mutatorSpan stats
  gc0Lines     = map parseLine $ gc0Span stats
  gc1Lines     = map parseLine $ gc1Span stats

parseLine :: String -> (String, Integer)
parseLine line =
  case words line of
    [event, count] -> (event, parseCount count)
    _              -> error ("Unexpected PAPI event line: "++line)

parseCount :: String -> Integer
parseCount = read . clean
  where
  clean :: String -> String
  clean []       = []
  clean (',':xs) = clean xs
  clean (' ':xs) = clean xs
  clean (c:xs)  | isDigit c = c : clean xs
  clean  xs      = error "unexpected character in PAPI count: "++xs

mutatorSpan :: [String] -> [String]
mutatorSpan = findPapiSpan "  Mutator CPU counters"

gc0Span :: [String] -> [String]
gc0Span = findPapiSpan "  GC(0) CPU counters"

gc1Span :: [String] -> [String]
gc1Span = findPapiSpan "  GC(1) CPU counters"

findPapiSpan :: String -> [String] -> [String]
findPapiSpan start =
  findSpan start "" ["           CYCLES"] 

findSpan :: String -> String -> [String] -> [String] -> [String]
findSpan startMarker endMarker skipList stats = 
  eat stats
  where
  eat  []                          = []
  eat (m:rest) | m == startMarker  = go rest
               | otherwise         = eat rest
  go   []                          = []
  go  (m:rest) | m == endMarker    = []
               | shouldSkip m      = go rest
               | otherwise         = m : go rest
  shouldSkip line = any (`isPrefixOf` line) skipList


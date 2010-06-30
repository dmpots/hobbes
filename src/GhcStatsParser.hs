module GhcStatsParser (
    PapiResult(..)
  , GhcPhaseData(..)
  , GhcPapiPhaseData 
  , parse
)
where
import Data.List
import Data.Char
import StatsFile

data GhcPhaseData a = GhcPhaseData {
    mutator :: a
  , gc0     :: a
  , gc1     :: a
  } deriving (Show)

instance Functor GhcPhaseData where
  fmap f p = GhcPhaseData {
                  mutator = (f . mutator) p
                , gc0     = (f . gc0)     p
                , gc1     = (f . gc1)     p
             }

type GhcPapiPhaseData = GhcPhaseData [(String, Integer)]

data PapiResult = PapiResult {
      statsFile    :: StatsFile
    , phaseResults :: GhcPapiPhaseData
  } deriving (Show)

parse :: StatsFile -> [String] -> PapiResult
parse sf stats = 
  PapiResult {
      statsFile = sf
    , phaseResults = GhcPhaseData {
          mutator   = map parseLine mutatorLines
        , gc0       = map parseLine gc0Lines 
        , gc1       = map parseLine gc1Lines 
      }
  }
  where
  mutatorLines = mutatorSpan stats
  gc0Lines     = gc0Span stats
  gc1Lines     = gc1Span stats

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


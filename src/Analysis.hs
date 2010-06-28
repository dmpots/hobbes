module Analysis (
    collect
  , dump
  , addSummaryData
)
where

import Data.Function
import Data.List
import Data.Maybe
import GhcStatsParser
import StatsFile

{----------------------------------------------------------
 - Types
 ---------------------------------------------------------}
type EventName         = String
type ComputedEventName = String
type SummaryFunMap     = [(String, SummaryFun)]
data Result = 
    RawResult      EventName         Integer 
  | ComputedResult ComputedEventName Double
  deriving (Show)

data SummaryFun = 
    ArithMean
  | GeoMean
  | Median
  deriving(Show)

data AnalysisResult = AnalysisResult {
      program        :: String
    , eventSet       :: [String]
    , fullResults    :: GhcPhaseData [[Result]]
    , summaryResults :: GhcPhaseData [Result]
  } deriving (Show)
type Selector a = GhcPhaseData a -> a


{----------------------------------------------------------
 - Data Consolidation
 ---------------------------------------------------------}
collect :: [PapiResult] -> [AnalysisResult]
collect papiResults = concatMap collectProgram programResults
  where
  programResults = groupBy ((==) `on` (progName . statsFile)) papiResults


collectProgram :: [PapiResult] -> [AnalysisResult]
collectProgram papiResults = map collectEventSet byEventSet
  where
  byEventSet = groupBy ((==) `on` (eventSetId . statsFile)) papiResults

collectEventSet :: [PapiResult] -> AnalysisResult
collectEventSet [] = error "Unexpected empty [PapiResult]"
collectEventSet papiResults@(r:_) = result
  where
  result  = AnalysisResult prog events full summary
  prog    = (progName . statsFile) r
  events  = map fst $ (mutator . phaseResults) r
  full    = GhcPhaseData (gather mutator) (gather gc0) (gather gc1)
  summary = GhcPhaseData [] [] []
  gather  p = map (\pr -> collectRawForPhase (phaseResults pr) p) papiResults

collectRawForPhase 
  ::  GhcPapiPhaseData 
  -> (GhcPapiPhaseData -> [(String, Integer)]) 
  ->  [Result]
collectRawForPhase phases phase = collectRaw (phase phases) 

collectRaw :: [(String, Integer)] -> [Result]
collectRaw = map (uncurry RawResult)


{----------------------------------------------------------
 - Summarizing Data
 ---------------------------------------------------------}
defaultSummaryFun :: SummaryFun 
defaultSummaryFun = Median

addSummaryData :: SummaryFunMap     -- ^ Map events to summary type
               -> [AnalysisResult]  -- ^ Current Results
               -> [AnalysisResult]  -- ^ Results with summaries
addSummaryData sumMap analysisResults = 
  map (addSummary sumMap) analysisResults

addSummary :: SummaryFunMap -> AnalysisResult -> AnalysisResult
addSummary sumMap analysisResult = 
  analysisResult { summaryResults = sumResults }
  where 
  sumResults = summaryForEvents sumMap events analysisResult
  events     = eventSet analysisResult

summaryForEvents :: SummaryFunMap -> [EventName] -> AnalysisResult -> GhcPhaseData [Result]
summaryForEvents sumMap eventNames analysisResult =
    GhcPhaseData {
        mutator = summarize mutator
      , gc0     = summarize gc0
      , gc1     = summarize gc1
    }
    where 
    summarize :: Selector [[Result]] -> [Result]
    summarize sel = map (getSummary sel) eventNames

    getSummary :: Selector [[Result]] -> EventName -> Result
    getSummary sel eventName = ComputedResult eventName (f datas)
     where datas = dataForEvent sel eventName
           f     = funForSummaryFun (getSummaryFunction sumMap eventName)

    dataForEvent :: Selector [[Result]] -> EventName -> [Double]
    dataForEvent selector e = map countAsDouble results
      where
      results = catMaybes events
      events = map (findEvent e) datas
      datas  = (selector . fullResults) analysisResult
  
  
funForSummaryFun:: SummaryFun -> ([Double] -> Double)
funForSummaryFun ArithMean = mean
  where mean [] = 0.0
        mean l  = (sum l) / (fromIntegral $ length l )
funForSummaryFun GeoMean   = geomean
  where geomean []   = 0.0
        geomean l    = (product l)** (1.0 / len)
          where len  = fromIntegral (length l)
          
funForSummaryFun Median    = median
  where median []    = 0.0
        median [x]   = x
        median [x,y] = avg x y
        median xs    = 
          let len      = length xs
              dlen     = fromIntegral len :: Double
              sorted   = sort xs
              lowerMid =  (floor (dlen / 2)) - 1
              upperMid =   floor (dlen / 2)    in
          if even len then 
                avg (sorted !! upperMid) (sorted !! lowerMid)
              else
                sorted !! upperMid
        avg a b = (a + b) / 2


  
getSummaryFunction :: SummaryFunMap -> String -> SummaryFun
getSummaryFunction sumMap eventName = 
  case lookup eventName sumMap of
    Just f  -> f
    Nothing -> defaultSummaryFun



{----------------------------------------------------------
 - Printing Functions
 ---------------------------------------------------------}
dump :: [AnalysisResult] -> IO ()
dump analysisData =
  mapM_ dumpIt analysisData

dumpIt :: AnalysisResult -> IO ()
dumpIt d = do
  putStrLn   "#"
  putStrLn $ "# "++(show $ program d) ++" " ++(show events)
  putStrLn   "#"
  putStrLn $ "n "++unwords events
  mapM_ printLine mutatorLines
  putStrLn $ "#" ++ (take 67 $ repeat '-')
  --putStrLn $ (show mutatorSummary)
  printLine mutatorSummary
  putStrLn ""
  where 
  events = eventSet d
  printLine :: Show header => ([Result], header) -> IO ()
  printLine (line, header) = do
    putStr $ (show header) ++ " "
    let out = map (\eventName -> outputForEvent eventName line) events
    mapM_ putStr (intersperse " " out)
    putStrLn ""

  outputForEvent :: EventName -> [Result] -> String
  outputForEvent e line = 
    case findEvent e line of
      Just c  -> showCount c
      Nothing -> "--"

  showCount (RawResult      _ cnt) = show cnt
  showCount (ComputedResult _ cnt) = show cnt

  mutatorLines   = zip ((mutator . fullResults) d) [(1::Int)..]
  mutatorSummary = (((mutator . summaryResults) d), 0 :: Int)

findEvent :: EventName -> [Result] -> Maybe Result
findEvent e = find (matchEvent e)

matchEvent :: EventName -> Result -> Bool
matchEvent e (RawResult      e' _) = e == e'
matchEvent e (ComputedResult e' _) = e == e'

countAsDouble :: Result -> Double
countAsDouble (RawResult      _ cnt) = fromInteger cnt
countAsDouble (ComputedResult _ cnt) = cnt
--dumpReports :: [AnalysisData] -> IO ()
--dumpReports analysisData =
  


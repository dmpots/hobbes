module Analysis (
    AnalysisResult
  , collect
  , dump
  , addSummaryData
  , addFormulaData
  , mergeProgramsForEvents
  , mergeEventsForProgram
  , dropRawData
)
where

import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Formula
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
    , formulasUsed   :: [Formula]
    , fullResults    :: GhcPhaseData [[Result]]
    , summaryResults :: GhcPhaseData [Result]
    , resultLabels   :: [String]
  } deriving (Show)
type Selector a = GhcPhaseData a -> a


{----------------------------------------------------------
 - Data Consolidation
 ---------------------------------------------------------}
collect :: [PapiResult] -> [AnalysisResult]
collect papiResults = concatMap collectProgram programResults
  where
  programResults = groupResultsBy (progName . statsFile) papiResults


collectProgram :: [PapiResult] -> [AnalysisResult]
collectProgram papiResults = map collectEventSet byEventSet
  where
  byEventSet = groupResultsBy (eventSetId . statsFile) papiResults

collectEventSet :: [PapiResult] -> AnalysisResult
collectEventSet [] = error "Unexpected empty [PapiResult]"
collectEventSet papiResults@(r:_) = result
  where
  result  = AnalysisResult prog events formula full summary labels
  prog    = (progName . statsFile) r
  events  = map fst $ (mutator . phaseResults) r
  full    = GhcPhaseData (gather mutator) (gather gc0) (gather gc1)
  summary = GhcPhaseData [] [] []
  formula = []
  labels  = map show ([1 .. (length (mutator full))])
  gather  p = map (\pr -> collectRawForPhase (phaseResults pr) p) papiResults

collectRawForPhase 
  ::  GhcPapiPhaseData 
  -> (GhcPapiPhaseData -> [(String, Integer)]) 
  ->  [Result]
collectRawForPhase phases phase = collectRaw (phase phases) 

collectRaw :: [(String, Integer)] -> [Result]
collectRaw = map (uncurry RawResult)

{----------------------------------------------------------
 - Data Grouping
 ---------------------------------------------------------}
mergeProgramsForEvents :: [AnalysisResult] -> [AnalysisResult]
mergeProgramsForEvents analysisResults = map merge byEventSet
  where
  byEventSet :: [[AnalysisResult]]
  byEventSet = groupResultsBy eventSet analysisResults

  merge :: [AnalysisResult] -> AnalysisResult
  merge []       = error "Unexpected empty list in merge"
  merge rs@(r:_) =  final
    where
    final   = r { program        = "Summary"
                , fullResults    = aggregatePhaseData (map summaryResults rs)
                , summaryResults = GhcPhaseData [] [] []
                , resultLabels   = map program rs}

mergeEventsForProgram :: [AnalysisResult] -> [AnalysisResult]
mergeEventsForProgram analysisResults = map merge byProgram
  where
  byProgram = groupResultsBy program analysisResults

  merge :: [AnalysisResult] -> AnalysisResult
  merge []       = error "unexpected empty list in program merge"
  merge rs@(r:_) = final
    where
    final = AnalysisResult {
                program        = (program r)
              , eventSet       = concatMap eventSet rs
              , formulasUsed   = concatMap formulasUsed rs
              , fullResults    = mergeFullData (map fullResults rs)
              , summaryResults = mergeSummaryData (map summaryResults rs)
              , resultLabels   = concatMap resultLabels rs
            }

groupResultsBy :: Ord k => Eq k =>
                  (a -> k)
               -> [a]
               -> [[a]]
groupResultsBy f analysisResults = groupBy eq (sortBy ord analysisResults)
  where
  eq  = ((==)    `on` f)
  ord = (compare `on` f)

aggregatePhaseData :: [GhcPhaseData  [Result]] -> GhcPhaseData [[Result]]
aggregatePhaseData = combinePhaseDataWith map

mergeFullData :: [GhcPhaseData [[Result]]] -> GhcPhaseData [[Result]]
mergeFullData = combinePhaseDataWith merge
  where
  merge sel d = ((map concat) . transpose) (map sel d)

mergeSummaryData :: [GhcPhaseData [Result]] -> GhcPhaseData [Result]
mergeSummaryData = combinePhaseDataWith concatMap

combinePhaseDataWith :: Show a => Show b => (Selector a -> t -> b) -> t -> GhcPhaseData b
combinePhaseDataWith combine phaseData =
  GhcPhaseData {
      mutator = combine mutator phaseData
    , gc0     = combine gc0     phaseData
    , gc1     = combine gc1     phaseData
  }

{----------------------------------------------------------
 - Summarizing Data
 ---------------------------------------------------------}
defaultSummaryFun :: SummaryFun 
defaultSummaryFun = ArithMean

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
 - Computing Formulas
 ---------------------------------------------------------}
addFormulaData :: [Formula] -> [AnalysisResult] -> [AnalysisResult]
addFormulaData formulas analysisResults = 
  map (addFormulaToResult formulas) analysisResults

addFormulaToResult :: [Formula] -> AnalysisResult -> AnalysisResult
addFormulaToResult [] analysisResult = analysisResult
addFormulaToResult fs analysisResult = 
  analysisResult { 
      eventSet     = events ++ formulaEvents
    , fullResults  = withFormulaResults
    , formulasUsed = (formulasUsed analysisResult) ++ computableFs
  }
  where
  withFormulaResults = addFormulas computableFs analysisResult
  computableFs       = findComputableFormulas fs events
  events             = eventSet analysisResult
  formulaEvents      = map (\(Formula f _) -> f) computableFs

addFormulas :: [Formula] -> AnalysisResult -> GhcPhaseData [[Result]]
addFormulas []       analysisResult = fullResults analysisResult
addFormulas formulas analysisResult = 
    GhcPhaseData {
        mutator = formulize mutator
      , gc0     = formulize gc0
      , gc1     = formulize gc1
    }
  where
  formulize :: Selector [[Result]] -> [[Result]]
  formulize sel = zipWith (++) curResults newResults
    where
    newResults = transpose $ map (computeFormula curResults) formulas
    curResults = (sel . fullResults) analysisResult

  computeFormula :: [[Result]] -> Formula -> [Result]
  computeFormula results formula = map (compute formula) results

  compute :: Formula -> [Result] -> Result
  compute (Formula f x) results = ComputedResult f (Formula.eval env x)
    where
    env = foldr createEnv (Map.empty) results
    createEnv (RawResult n i)      = Map.insert n (fromInteger i)
    createEnv (ComputedResult n d) = Map.insert n d
  


findComputableFormulas :: [Formula] -> [EventName] -> [Formula]
findComputableFormulas formulas events = computable
  where 
  computable = filter (canEval env) formulas
  env        = Map.fromList (zip events zeros)
  zeros      = repeat 0.0


{----------------------------------------------------------
 - Filtering Functions
 ---------------------------------------------------------}
dropRawData :: [AnalysisResult] -> [AnalysisResult]
dropRawData analysisResults = map keepFormulaEvents analysisResults

keepFormulaEvents :: AnalysisResult -> AnalysisResult
keepFormulaEvents analysisResult = keepEventsIn formulaEvents analysisResult
  where
  formulaEvents = map (\(Formula n _) -> n) (formulasUsed analysisResult)

keepEventsIn :: [EventName] -> AnalysisResult -> AnalysisResult
keepEventsIn eventsToKeep r =
  r {
        eventSet       = filter keepEvent (eventSet r)
      , fullResults    = filterFullPhaseData    keepResult (fullResults r)
      , summaryResults = filterSummaryPhaseData keepResult (summaryResults r)
  }
  where
  keepEvent  = (\n -> n `elem` eventsToKeep)
  keepResult = (\result -> keepEvent (resultEvent result))

filterFullPhaseData :: (Result -> Bool)
                    -> GhcPhaseData [[Result]]
                    -> GhcPhaseData [[Result]]
filterFullPhaseData f = filterPhaseData (map (filter f))

filterSummaryPhaseData :: (Result -> Bool)
                       -> GhcPhaseData [Result]
                       -> GhcPhaseData [Result]
filterSummaryPhaseData f = filterPhaseData (filter f)

filterPhaseData :: (a -> a) -> GhcPhaseData a -> GhcPhaseData a
filterPhaseData = fmap

resultEvent :: Result -> EventName
resultEvent (RawResult      n _) = n
resultEvent (ComputedResult n _) = n

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
  mapM_ (\f  -> putStr "# " >> (putStrLn . show) f) (formulasUsed d)
  putStrLn   "#"
  putStrLn $ "n "++unwords events
  mapM_ printLine mutatorLines
  putStrLn $ "#" ++ (take 67 $ repeat '-')
  putStrLn $ "# " ++ show summaryFuns
  putStr "# " >> printLine mutatorSummary
  putStrLn ""
  where 
  events = eventSet d
  printLine :: Show h => ([Result], Maybe h) -> IO ()
  printLine (line, header) = do
    case header of 
      Just s  -> do {putStr $ (show s) ++ " "}
      Nothing -> return ()
    let out = map (\eventName -> outputForEvent eventName line) events
    mapM_ putStr (intersperse " " out)
    putStrLn ""

  outputForEvent :: EventName -> [Result] -> String
  outputForEvent e line = 
    case findEvent e line of
      Just c  -> showCount c
      Nothing -> "--"

  summaryFuns = map (getSummaryFunction []) events

  showCount (RawResult      _ cnt) = show cnt
  showCount (ComputedResult _ cnt) = show cnt

  mutatorLines   = zip ((mutator . fullResults) d) (map Just (resultLabels d))
  mutatorSummary = (((mutator . summaryResults) d), Nothing :: Maybe Int)

findEvent :: EventName -> [Result] -> Maybe Result
findEvent e = find (matchEvent e)

matchEvent :: EventName -> Result -> Bool
matchEvent e (RawResult      e' _) = e == e'
matchEvent e (ComputedResult e' _) = e == e'

countAsDouble :: Result -> Double
countAsDouble (RawResult      _ cnt) = fromInteger cnt
countAsDouble (ComputedResult _ cnt) = cnt

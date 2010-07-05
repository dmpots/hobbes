module Analysis (
    AnalysisResult
  , collect
  , dump
  , addSummaryData
  , addFormulaData
  , mergeProgramsForEvents
  , mergeEventsForProgram
  , dropRawData
  , dumpOnlyPhase
)
where

import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Formula
import PhaseData
import PapiResult

{----------------------------------------------------------
 - Types
 ---------------------------------------------------------}
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
    , fullResults    :: PhaseData [[Result]]
    , summaryResults :: PhaseData [Result]
    , resultLabels   :: [String]
  } deriving (Show)

{----------------------------------------------------------
 - Data Consolidation
 ---------------------------------------------------------}
collect :: Ord a => [PapiResult a] -> [AnalysisResult]
collect papiResults = concatMap collectProgram programResults
  where
  programResults = groupResultsBy programName papiResults


collectProgram :: Ord a => [PapiResult a] -> [AnalysisResult]
collectProgram papiResults = map collectEventSet byEventSet
  where
  byEventSet = groupResultsBy papiEvents papiResults

collectEventSet :: Ord a => [PapiResult a] -> AnalysisResult
collectEventSet [] = error "Unexpected empty [PapiResult]"
collectEventSet papiResults@(r:_) = result
  where
  result  = AnalysisResult prog events formula full summary labels
  prog    = programName r
  full    = aggregatePhaseData $ map (collectRaw . phaseResults) papiResults
  summary = PhaseData.emptyPhaseData
  formula = []
  labels  = map show ([1 .. (length papiResults)])
  events  = case phaseNames phaseR of
                  []    -> []
                  (p:_) -> map fst (fromMaybe [] (lookupPhase p phaseR))
  phaseR  = phaseResults r

collectRaw :: PhaseData [(String, Integer)] -> PhaseData [Result]
collectRaw = fmap (map (uncurry RawResult))

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
                , summaryResults = PhaseData.emptyPhaseData
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

aggregatePhaseData :: [PhaseData  [Result]] -> PhaseData [[Result]]
aggregatePhaseData = combinePhaseDataWith combine
  where
  combine :: PhaseData [Result] -> PhaseData [[Result]] -> PhaseData[[Result]]
  combine phaseData res = PhaseData $
    Map.unionWith (++) (Map.map (\e->[e]) $ toMap phaseData) (toMap res)

mergeFullData :: [PhaseData [[Result]]] -> PhaseData [[Result]]
mergeFullData = combinePhaseDataWith merge
  where
  merge :: PhaseData[[Result]] -> PhaseData[[Result]] -> PhaseData[[Result]]
  merge phaseData res = PhaseData $
    Map.unionWith (zipWith (++)) (toMap phaseData) (toMap res)

mergeSummaryData :: [PhaseData [Result]] -> PhaseData [Result]
mergeSummaryData = combinePhaseDataWith merge
  where
  merge :: PhaseData[Result] -> PhaseData[Result] -> PhaseData[Result]
  merge phaseData res = PhaseData $
    Map.unionWith (++) (toMap phaseData) (toMap res)

combinePhaseDataWith :: (PhaseData a -> PhaseData b -> PhaseData b)
                     -> [PhaseData a]
                     ->  PhaseData b
combinePhaseDataWith combine = foldr combine emptyPhaseData

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

summaryForEvents :: SummaryFunMap -> [EventName] -> AnalysisResult -> PhaseData [Result]
summaryForEvents sumMap eventNames analysisResult =
    PhaseData (Map.map summarize (toMap.fullResults $ analysisResult))
    where 
    summarize :: [[Result]] -> [Result]
    summarize res = map (getSummary res) eventNames

    getSummary :: [[Result]] -> EventName -> Result
    getSummary res eventName = ComputedResult eventName (f datas)
     where datas = dataForEvent res eventName
           f     = funForSummaryFun (getSummaryFunction sumMap eventName)

    dataForEvent :: [[Result]] -> EventName -> [Double]
    dataForEvent res e = map countAsDouble results
      where
      results = catMaybes events
      events = map (findEvent e) res
  
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

addFormulas :: [Formula] -> AnalysisResult -> PhaseData [[Result]]
addFormulas []       analysisResult = fullResults analysisResult
addFormulas formulas analysisResult = 
    PhaseData $ Map.map formulize ((toMap . fullResults) analysisResult)
  where
  formulize :: [[Result]] -> [[Result]]
  formulize res = zipWith (++) res newResults
    where
    newResults = transpose $ map (computeFormula res) formulas

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
                    -> PhaseData [[Result]]
                    -> PhaseData [[Result]]
filterFullPhaseData f = filterPhaseData (map (filter f))

filterSummaryPhaseData :: (Result -> Bool)
                       -> PhaseData [Result]
                       -> PhaseData [Result]
filterSummaryPhaseData f = filterPhaseData (filter f)

filterPhaseData :: (a -> a) -> PhaseData a -> PhaseData a
filterPhaseData = fmap

resultEvent :: Result -> EventName
resultEvent (RawResult      n _) = n
resultEvent (ComputedResult n _) = n

{----------------------------------------------------------
 - Printing Functions
 ---------------------------------------------------------}
dump :: [AnalysisResult] -> IO ()
dump analysisData =
  mapM_ dumpPhases analysisData

dumpOnlyPhase :: PhaseName -> [AnalysisResult] -> IO ()
dumpOnlyPhase n = mapM_ (flip dumpPhase n)

dumpPhases :: AnalysisResult -> IO ()
dumpPhases analysisResult = mapM_ (dumpPhase analysisResult) phases
  where
  phases      = Set.toList $ Set.union fullKeys summKeys
  fullKeys    = keysSet fullResults
  summKeys    = keysSet summaryResults
  keysSet sel = Map.keysSet ((toMap.sel) analysisResult)

dumpPhase :: AnalysisResult -> PhaseName -> IO ()
dumpPhase analysisResult phase = dumpIt full summ analysisResult
  where
  full      = case findP fullResults    of Just f -> f; Nothing -> [[]]
  summ      = case findP summaryResults of Just s -> s; Nothing ->  []
  findP sel = Map.lookup phase ((toMap . sel) analysisResult)

dumpIt :: [[Result]] -> [Result] -> AnalysisResult -> IO ()
dumpIt full summ d = do
  putStrLn   "#"
  putStrLn $ "# "++(show $ program d) ++" " ++(show events)
  mapM_ (\f  -> putStr "# " >> (putStrLn . show) f) (formulasUsed d)
  putStrLn   "#"
  putStrLn $ "n\t"++(concat $ intersperse "\t" events)
  mapM_ printLine fullLines
  putStrLn $ "#" ++ (take 67 $ repeat '-')
  putStrLn $ "# "++unwords events
  putStrLn $ "# " ++ show summaryFuns
  putStr "# " >> printLine summLines
  putStrLn ""
  where 
  events = eventSet d
  printLine :: Show h => ([Result], Maybe h) -> IO ()
  printLine (line, header) = do
    case header of 
      Just s  -> do {putStr $ (show s) ++ "\t"}
      Nothing -> return ()
    let out = map (\eventName -> outputForEvent eventName line) events
    mapM_ putStr (intersperse "\t" out)
    putStrLn ""

  outputForEvent :: EventName -> [Result] -> String
  outputForEvent e line = 
    case findEvent e line of
      Just c  -> showCount c
      Nothing -> "--"

  summaryFuns = map (getSummaryFunction []) events

  showCount (RawResult      _ cnt) = show cnt
  showCount (ComputedResult _ cnt) = show cnt

  fullLines = zip full (map Just (resultLabels d))
  summLines = (summ, Nothing :: Maybe Int)

findEvent :: EventName -> [Result] -> Maybe Result
findEvent e = find (matchEvent e)

matchEvent :: EventName -> Result -> Bool
matchEvent e (RawResult      e' _) = e == e'
matchEvent e (ComputedResult e' _) = e == e'

countAsDouble :: Result -> Double
countAsDouble (RawResult      _ cnt) = fromInteger cnt
countAsDouble (ComputedResult _ cnt) = cnt

{-# OPTIONS -fno-warn-missing-signatures #-}
module XmlStatsWriter(
  printResults
)
where
import Analysis
import Data.Maybe
import PhaseData
import Text.XML.HXT.Arrow
import Data.List

resultsXml rs = selem "results" $ map programXml rs

programXml p =
  mkelem "program"
  [sattr "name" (program p)]
  (eventSetXml (eventSet p) p : measurementsXml (fullResults p))

eventSetXml es programResults =
  selem "eventSet" $ concat [
      map (\e -> selem "eventName"       [txt e]) events
    , map (\e -> selem "computationName" [txt e]) computations]
  where
  (computations, events) = partition (isComputedEvent programResults) es

measurementsXml phaseRs =
  let phaseData = catMaybes $ map getPhaseData (phaseNames phaseRs) in
  measurementXml phaseData
  where
  getPhaseData pn =
    case lookupPhase pn phaseRs of Just a -> Just (pn, a); _ -> Nothing

measurementXml []     = []
measurementXml phases =
  let (next, remaining) = nextMeasurement in
  selem "measurement" (map phaseXml next) : measurementXml remaining
  where
  nextMeasurement ::([(PhaseName, [Result])],  -- next
                     [(PhaseName, [[Result]])]) -- remaining
  nextMeasurement = go phases [] []
  go []              nexts rems = (nexts, rems)
  go ((_,    []):ps) nexts rems = go ps          nexts            rems
  go ((pn, r:[]):ps) nexts rems = go ps ((pn, r):nexts)           rems
  go ((pn, r:rs):ps) nexts rems = go ps ((pn, r):nexts) ((pn, rs):rems)


phaseXml (p , es) =
  mkelem "phase" [sattr "name" p] (map eventXml es)

eventXml (RawResult n c) =
  mkelem "event" [sattr "name" n] [txt $ show c]
eventXml (ComputedResult n d) =
  mkelem "computation" [sattr "name" n] [txt $ show d]

printResults :: [AnalysisResult] -> IO ()
printResults rs = do
  runX (
        root [] [resultsXml rs]
        >>>
        writeDocument [(a_indent, v_1)] ""
       )
  >>
  return ()

{-
-- testing function
test :: IO ()
test = do
  xrs <- XmlStatsParser.parseFile "T.xml"
  let rs  = Analysis.collect xrs
  printResults rs
-}


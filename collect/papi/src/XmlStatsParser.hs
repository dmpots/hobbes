{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
{-
    This parser is modeled after the example at:
      http://www.haskell.org/haskellwiki/HXT/Practical/Simple2
      and
      http://www.haskell.org/haskellwiki/HXT/Practical/Weather1
-}
module XmlStatsParser(
    parseFile
)
where
import qualified Data.Set as Set
import PapiResult
import PhaseData
import Text.XML.HXT.Arrow

atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText
textAtTag tag = atTag tag >>> text

getPrograms = atTag "program" >>>
  proc p -> do
    pName    <- getAttrValue "name"      -< p
    eventSet <- getEventSet              -< p
    measurements <- listA getMeasurement -< p
    returnA -< map (\ms ->
                      let phases = createPhaseDataWithPhases ms in
                        PapiResult {
                            programName  = pName
                          , papiEvents   = Set.fromList eventSet
                          , phaseResults = phases
                        }
                ) measurements

getEventSet = atTag "eventSet" >>>
  proc es -> do
    events <- listA getEventNames -< es
    returnA -< events

getEventNames = textAtTag "eventName"

getMeasurement = atTag "measurement" >>>
  proc m -> do
    phases <- listA getPhase -< m
    returnA -< phases

getPhase = atTag "phase" >>>
  proc p -> do
    phaseName  <- getAttrValue "name" -< p
    events     <- listA getEvent      -< p
    returnA -< (phaseName, events)

getEvent = atTag "event" >>>
  proc e -> do
    n <- getAttrValue "name" -< e
    v <- text                -< e
    returnA -< (n, read v :: Integer)

parseXML file = readDocument [(a_validate,v_0)] file

{-
-- For testing
parse = do
  x <- runX (parseXML "T.xml" >>> getPrograms)
  print (concat x)
-}

parseFile :: FilePath -> IO [PapiResult EventName]
parseFile file = do
  x <- runX (parseXML file >>> getPrograms)
  return (concat x)


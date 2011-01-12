module PapiResult (
    PapiResult(..)
  , EventName
  , EventCount
)
where
import PhaseData
import qualified Data.Set as Set

type EventName  = String
type EventCount = Integer

-- | Representaion for results collected from PAPI measurements
data PapiResult a = PapiResult {
      -- | The program for which the results were collected
      programName   :: String    
      -- | The set of events for which we have results
    , papiEvents   :: Set.Set a 
      -- | The results for each phase of the program
    , phaseResults :: PhaseData [(EventName, EventCount)]
  } deriving (Show)




module PhaseData (
    PhaseName
  , PhaseData(..)
  , emptyPhaseData 
  , addPhase
  , lookupPhase
  , phaseNames
)
where

import qualified Data.Map as Map

type PhaseName  = String
newtype PhaseData a = PhaseData {toMap :: Map.Map PhaseName a}
  deriving (Show)

instance Functor PhaseData where
  fmap f d = PhaseData $ Map.map f (toMap d)

lookupPhase :: PhaseName -> PhaseData a -> Maybe a
lookupPhase n d = Map.lookup n (toMap d)

addPhase :: PhaseName -> a -> PhaseData a -> PhaseData a
addPhase n a d = d {toMap =  Map.insert n a (toMap d)}

emptyPhaseData :: PhaseData a
emptyPhaseData = PhaseData (Map.empty)

phaseNames :: PhaseData a -> [PhaseName]
phaseNames = (Map.keys . toMap)


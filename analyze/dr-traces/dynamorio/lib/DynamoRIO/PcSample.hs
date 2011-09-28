module DynamoRIO.PcSample(PcSample(..), SampleLoc(..), Offset, SampleCount)
where

import DynamoRIO.Trace(Addr)
import Data.Word

data SampleLoc =
    AppLoc 
  | InterpLoc 
  | TraceLoc Offset 
  | BlockLoc 
  | DispatchLoc 
  | MonitorLoc 
  | SignalHandlerLoc 
  | SyscallHandlerLoc 
  | ContextSwitchLoc 
  | IblLoc
  | UnknownLoc
  deriving(Show, Eq)

type Offset = Addr
type SampleCount = Word64

data PcSample = PcSample {
      sampleAddr  :: Addr
    , sampleCount :: SampleCount
    , sampleLoc   :: SampleLoc
  } deriving(Show)


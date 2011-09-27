module DynamoRIO.Trace(
    Trace(..)
  , BasicBlock
  , Addr
  , AppAddr
  , CacheAddr
  , TraceId
  , TraceLayout(..)
  , TraceEntry(..)
  , TraceExit(..)
) where

import Data.Word

-- Trace Text Parsing
data Trace = Trace {
      traceId     :: TraceId
    , traceTag    :: AppAddr
    , traceBlocks :: [BasicBlock]
    , traceLayout :: TraceLayout
  } deriving(Show)

type TraceId     = Int
type BasicBlock  = AppAddr
type Addr        = Word64
type AppAddr     = Addr    -- ^ Application Address
type CacheAddr   = Addr    -- ^ Fragment Cache Address
type DRAddr      = Addr    -- ^ DynamoRIO Address

data TraceEntry = 
    IndirectBranchEntry {entryAddr :: CacheAddr}
  | PrefixEntry         {entryAddr :: CacheAddr}
  | NormalEntry         {entryAddr :: CacheAddr}
  deriving(Show)

data TraceExit  = 
    ExitStub   CacheAddr
  | ExitLinked CacheAddr
  | ExitIbl    DRAddr IblRoutine
  deriving(Show)

type IblRoutine = String

data TraceLayout = 
  TraceLayout {
      traceEntries :: [TraceEntry]
    , traceExits :: [TraceExit]
  }
  deriving(Show)

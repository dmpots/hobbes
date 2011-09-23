module DynamoRIO.Trace(
    Trace(..)
  , BasicBlock
  , Addr
  , TraceId
) where

import Data.Word

-- Trace Text Parsing
data Trace = Trace TraceId [BasicBlock] deriving(Show)
type BasicBlock  = Addr
type Addr        = Word64
type TraceId     = Int


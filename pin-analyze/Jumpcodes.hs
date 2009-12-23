module Jumpcodes where

data Jump =
   CallSeen    |  CallTaken |
  ICallSeen    | ICallTaken |
   BranchSeen  |  BranchTaken |
  IBranchSeen  | IBranchTaken |
   SyscallSeen | SyscallTaken |
   ReturnSeen  | ReturnTaken
  
  deriving (Eq, Enum, Ord, Read, Show)

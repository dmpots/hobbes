module OpcodeType where

data OpcodeType =
    DataMovement
  | Stack
  | Arithmetic
  | Logical
  | ShiftRotate
  | Bit
  | Branch
  | X87FPU
  | SIMDFp
  | SIMDInt
  | SSE
  | Other
  deriving (Show, Read, Eq, Enum, Ord)



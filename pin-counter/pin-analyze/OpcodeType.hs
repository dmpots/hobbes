module OpcodeType where

data OpcodeType =
    DataMovement
  | Stack
  | Arithmetic
  | Logical
  | ShiftRotate
  | Bit
  | Branch
  | SSE
  | Other
  | X87FPU
  | SIMDFp
  | SIMDInt
  deriving (Show, Read, Eq, Enum, Ord)



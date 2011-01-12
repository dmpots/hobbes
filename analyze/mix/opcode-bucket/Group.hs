module Group where

import Opcodes
import OpcodeType

data OpcodeGroup = OpcodeGroup {
    opcodeS :: String
  --, opcodeType :: [String]
  , opcodeType :: OpcodeType
} deriving (Show, Eq)


assignGroup :: [String] -> OpcodeType
assignGroup groups | "sse1"    `elem` groups = SSE
assignGroup groups | "sse2"    `elem` groups = SSE
assignGroup groups | "sse3"    `elem` groups = SSE
assignGroup groups | "sse4"    `elem` groups = SSE
assignGroup groups | "mmx"     `elem` groups = SSE
assignGroup groups | "simdfp"  `elem` groups = SIMDFp
assignGroup groups | "simdint" `elem` groups = SIMDInt
assignGroup groups | "x87fpu"  `elem` groups = X87FPU
assignGroup groups | "branch"  `elem` groups = Branch
assignGroup groups | "bit"     `elem` groups = Bit
assignGroup groups | "shftrot" `elem` groups = ShiftRotate
assignGroup groups | "logical" `elem` groups = Logical
assignGroup groups | "arith"   `elem` groups = Arithmetic
assignGroup groups | "stack"   `elem` groups = Stack
assignGroup groups | "datamov" `elem` groups = DataMovement
assignGroup _      | otherwise               = Other


isSanctionedOpcode :: String -> Bool
isSanctionedOpcode oc = validParse (reads oc)
  where
  validParse :: [(Opcode,String)] -> Bool
  validParse [(_,"")] = True
  validParse _        = False

mkAssignCase :: OpcodeGroup -> Maybe String
mkAssignCase group | isSanctionedOpcode opcode = 
  Just ("opcodeGroup " ++ opcode ++ " = " ++ ( (show . opcodeType) group))
  where opcode = opcodeS group
mkAssignCase _     | otherwise                 = 
  Nothing



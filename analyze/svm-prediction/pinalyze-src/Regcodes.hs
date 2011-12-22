module Regcodes where


data Reg =
  RDI_R | RDI_W |
  RSI_R | RSI_W |
  RBP_R | RBP_W |
  RSP_R | RSP_W |
  RBX_R | RBX_W |
  RDX_R | RDX_W |
  RCX_R | RCX_W |
  RAX_R | RAX_W |
  R8_R  | R8_W  |
  R9_R  | R9_W  |
  R10_R | R10_W |
  R11_R | R11_W |
  R12_R | R12_W |
  R13_R | R13_W |
  R14_R | R14_W |
  R15_R | R15_W  
  deriving (Eq, Enum, Ord, Read, Show)

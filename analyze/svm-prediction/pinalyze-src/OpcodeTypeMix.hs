module OpcodeTypeMix where
import OpcodeMix
import OpcodeType
import OpcodeGroup
import PinData

readOpcodeTypeCount :: String -> (OpcodeType, PinCounter)
readOpcodeTypeCount s = (opcodeGroup op, cnt) 
  where
  (op,cnt) = readOpcodeCount s


module OpcodeMix where
import Opcodes
import PinData

type OpId    = Int
type PinOpcodeData = GenCountData Opcode

readOpcodeCount :: String -> (Opcode, PinCounter)
readOpcodeCount line = 
  if opId /= (fromEnum opcode) then errorOut else (opcode, count)
  where 
  (opId, opcode, count) = read line :: (OpId, Opcode, PinCounter)
  errorOut = error errMsg
                   
  errMsg   = "Suspect data!\nID: "++(show opId)++" does not match "
             ++ "opcode enum for "++(show opcode)



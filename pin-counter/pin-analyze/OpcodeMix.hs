module OpcodeMix where
import Opcodes
import PinData
import Text.Regex

type OpId    = Int
type PinOpcodeData = GenCountData Opcode

opcountRegex :: Regex
opcountRegex = mkRegex "\\(([0-9]+), *[A-Za-z0-9_]+, *([0-9]+)\\)"

readOpcodeCount :: String -> (Opcode, PinCounter)
readOpcodeCount line = (toEnum opcode, count)
  where 
  Just [opId, opcount] = matchRegex opcountRegex line
  opcode = read opId    :: Int
  count  = read opcount :: Integer


-- this definition of readOpcodeCount is way too slow 100x slower than above
--readOpcodeCount line = (toEnum opcode, count)
  --if opId /= (fromEnum opcode) then errorOut else (opcode, count)
--where
  --(opId, opcode, count) = read line :: (OpId, Opcode, PinCounter)
  --errorOut = error errMsg
  --errMsg   = "Suspect data!\nID: "++(show opId)++" does not match "
  --           ++ "opcode enum for "++(show opcode)



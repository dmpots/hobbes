module OpcodeMix where
import Opcodes
import Data.Set(Set) 
import qualified Data.Set as Set
import Data.Map(Map) 
import qualified Data.Map as Map

type OpId    = Int
type OpCount = Integer
type BenchmarkName = String
type OpCounts  = (Opcode, OpCount)

data GenPinOpCodeData a = OpData { bmName :: String, opCounts :: a}
type PinOpCodeData = GenPinOpCodeData [OpCounts]
type OpcodeMap = Map Opcodes.Opcode OpCount
type PinOpCodeMapData = GenPinOpCodeData OpcodeMap


fillMissingData :: [PinOpCodeData] -> [PinOpCodeData]
fillMissingData pinData = map fill mapData
    where
    opcodes = Set.toList (collectAllOpCodes pinData)
    mapData = opCountsToMap pinData
    fill d@(OpData n opMap)  = d{opCounts = map (countOrZero opMap) opcodes}
    countOrZero opMap opcode = 
        case Map.lookup opcode opMap of
            Just count -> (opcode, count)
            Nothing    -> (opcode, 0)

opCountsToMap :: [PinOpCodeData] -> [PinOpCodeMapData]
opCountsToMap pinData = map transform pinData
    where
    transform  d = d { opCounts = Map.fromList (opCounts d)}

collectAllOpCodes :: [PinOpCodeData] -> Set Opcodes.Opcode
collectAllOpCodes pinData = Set.fromList allOpCodes
    where 
    allOpCodes = map fst (concatMap opCounts pinData)



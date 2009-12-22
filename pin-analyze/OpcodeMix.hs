module OpcodeMix where
import Opcodes
import ClusterElement
import PinData
import Data.Set(Set) 
import qualified Data.Set as Set
import Data.Map(Map) 
import qualified Data.Map as Map

type OpId    = Int
type OpCount = Integer
type BenchmarkName = String
type OpCounts  = (Opcode, OpCount)


type PinOpCodeData = GenPinData [OpCounts]
type OpcodeMap = Map Opcode OpCount
type PinOpCodeMapData = GenPinData OpcodeMap

data AnalysisData = AnalysisData {
      label         :: AnalysisLabel
    , rawCount      :: OpCount
    , percentTotal  :: Double
} deriving(Show, Read)
type PinOpCodeAnalysisData = GenPinData [AnalysisData]
type POAD = PinOpCodeAnalysisData 
data AnalysisLabel = StringLabel String | OpcodeLabel Opcode
    deriving (Eq, Show, Read, Ord)

readCount :: String -> OpCounts
readCount line = 
  if opId /= (fromEnum opcode) then errorOut else (opcode, count)
  where 
  (opId, opcode, count) = read line :: (OpId, Opcode, OpCount)
  errorOut = error errMsg
                   
  errMsg   = "Suspect data!\nID: "++(show opId)++" does not match "
             ++ "opcode enum for "++(show opcode)


fillMissingData :: [PinOpCodeData] -> [PinOpCodeData]
fillMissingData allPinData = map fill mapData
    where
    opcodes = collectAllOpCodes allPinData
    mapData = opCountsToMap allPinData
    fill d  = d {pinData = map (countOrZero (pinData d)) opcodes}
    countOrZero opMap opcode = 
        case Map.lookup opcode opMap of
            Just count -> (opcode, count)
            Nothing    -> (opcode, 0)

opCountsToMap :: [PinOpCodeData] -> [PinOpCodeMapData]
opCountsToMap = map transform 
    where
    transform  d = d { pinData = Map.fromList (pinData d)}

collectAllOpCodes :: [PinOpCodeData] -> [Opcode]
collectAllOpCodes allPinData = Set.toList . Set.fromList $ allOpCodes
    where 
    allOpCodes = map fst (concatMap pinData allPinData)


convertToAnalysisData :: [PinOpCodeData] -> [PinOpCodeAnalysisData]
convertToAnalysisData counts = 
  zipWith3 PinData bmNames bmLabels analysisData
  where
  analysisData  = zipWith (zipWith convert) bmOpCounts percentCounts 
                                                :: [[AnalysisData]]
  bmNames       = map bmName  counts            :: [String]
  bmLabels      = map bmLabel counts            :: [ProgramClass]
  percentCounts = map percentsOfTotal rawCounts :: [[Double]]
  rawCounts     = map (map snd) bmOpCounts      :: [[Integer]]
  bmOpCounts    = map pinData counts           :: [[OpCounts]]
  convert (code, count) percent = 
      AnalysisData { 
          label        = OpcodeLabel code
        , rawCount     = count
        , percentTotal = percent
      }

percentsOfTotal :: [OpCount] -> [Double]
percentsOfTotal counts = map ((/total).fromIntegral) counts
    where total = fromIntegral (sum counts)


dropUnimportantData :: Double -> [POAD] ->  [POAD] 
dropUnimportantData threshold analysisData = 
  map filterData analysisData
  where
  filterData opcodeData =
    let opcodeFilter = (\aData -> Set.member (label aData) chosenOnes) in
    opcodeData {
        pinData = filter opcodeFilter (pinData opcodeData)
    }
    
  sets = map (chooseImportantOpCodes threshold) analysisData
  chosenOnes = foldr Set.union Set.empty sets :: Set AnalysisLabel


chooseImportantOpCodes :: Double -> PinOpCodeAnalysisData -> Set AnalysisLabel
chooseImportantOpCodes threshold  d =
  foldr unionIf (Set.empty) (pinData d)
  where
  unionIf aData set =
    if (percentTotal aData) > threshold 
    then Set.insert (label aData) set 
    else set




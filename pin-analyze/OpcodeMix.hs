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
type OpcodeMap = Map Opcode OpCount
type PinOpCodeMapData = GenPinOpCodeData OpcodeMap

data AnalysisData = AnalysisData {
      label         :: AnalysisLabel
    , rawCount      :: OpCount
    , percentTotal  :: Double
}
type PinOpCodeAnalysisData = GenPinOpCodeData [AnalysisData]
type POAD = PinOpCodeAnalysisData 
data AnalysisLabel = StringLabel String | OpcodeLabel Opcode
    deriving (Eq, Show, Ord)

fillMissingData :: [PinOpCodeData] -> [PinOpCodeData]
fillMissingData pinData = map fill mapData
    where
    opcodes = collectAllOpCodes pinData
    mapData = opCountsToMap pinData
    fill d@(OpData n opMap)  = d {opCounts = map (countOrZero opMap) opcodes}
    countOrZero opMap opcode = 
        case Map.lookup opcode opMap of
            Just count -> (opcode, count)
            Nothing    -> (opcode, 0)

opCountsToMap :: [PinOpCodeData] -> [PinOpCodeMapData]
opCountsToMap pinData = map transform pinData
    where
    transform  d = d { opCounts = Map.fromList (opCounts d)}

collectAllOpCodes :: [PinOpCodeData] -> [Opcode]
collectAllOpCodes pinData = Set.toList . Set.fromList $ allOpCodes
    where 
    allOpCodes = map fst (concatMap opCounts pinData)


convertToAnalysisData :: [PinOpCodeData] -> [PinOpCodeAnalysisData]
convertToAnalysisData counts = 
  zipWith OpData bmNames analysisData
  where
  analysisData  = zipWith (zipWith convert) bmOpCounts percentCounts 
                                                :: [[AnalysisData]]
  bmNames       = map bmName counts             :: [String]
  percentCounts = map percentsOfTotal rawCounts :: [[Double]]
  rawCounts     = map (map snd) bmOpCounts      :: [[Integer]]
  bmOpCounts    = map opCounts counts           :: [[OpCounts]]
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
        opCounts = filter opcodeFilter (opCounts opcodeData)
    }
    
  sets = map (chooseImportantOpCodes threshold) analysisData
  chosenOnes = foldr Set.union Set.empty sets :: Set AnalysisLabel


chooseImportantOpCodes :: Double -> PinOpCodeAnalysisData -> Set AnalysisLabel
chooseImportantOpCodes threshold  (OpData bm analysisData) =
  foldr unionIf (Set.empty) analysisData 
  where
  unionIf aData set =
    if (percentTotal aData) > threshold 
    then Set.insert (label aData) set 
    else set




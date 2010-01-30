module PinData where
import ClusterElement
import Data.Set(Set) 
import qualified Data.Set as Set
import qualified Data.Map as Map
import Opcodes
import Jumpcodes
import Regcodes

data GenPinData a = PinData { 
      bmName  :: String
    , bmLabel :: ProgramClass
    , pinData :: a
} deriving(Show, Read)

data PinTool = OpcodeMix | JumpMix | RegMix | BBLengthMix
  deriving(Eq, Enum, Show, Read)

type PinCounter = Integer
type GenCountData k  = GenPinData [(k,PinCounter)]
type PinAnalysisData = GenPinData [AnalysisData]

data AnalysisData = AnalysisData {
      label         :: AnalysisLabel
    , rawCount      :: PinCounter
    , percentTotal  :: Double
} deriving(Show, Read)

data AnalysisLabel = 
    OpcodeLabel   Opcode
  | JumpLabel     Jump
  | RegLabel      Reg
  | BBLengthLabel Int
    deriving (Eq, Show, Read, Ord)

alEnum :: AnalysisLabel -> Int
alEnum (OpcodeLabel oc)   = fromEnum oc
alEnum (JumpLabel   jl)   = fromEnum jl
alEnum (RegLabel    rl)   = fromEnum rl
alEnum (BBLengthLabel ll) =          ll

fillMissingData :: Ord k => [GenCountData k] -> [GenCountData k]
fillMissingData allPinData = zipWith fill allPinData mapData
    where
    opcodes = collectAllKeys allPinData
    mapData = map (\d -> Map.fromList (pinData d)) allPinData
    fill d md = d {pinData = map (countOrZero md) opcodes}
    countOrZero opMap opcode = 
        case Map.lookup opcode opMap of
            Just count -> (opcode, count)
            Nothing    -> (opcode, 0)

collectAllKeys :: Ord k => [GenCountData k] -> [k]
collectAllKeys allPinData = Set.toList . Set.fromList $ allOpCodes
    where 
    allOpCodes = map fst (concatMap pinData allPinData)

convertToAnalysisData :: [GenCountData k] 
  -> (k -> AnalysisLabel)
  -> [PinAnalysisData]
convertToAnalysisData counts mkLabel =
  zipWith3 PinData bmNames bmLabels analysisData
  where
  analysisData  = zipWith (zipWith convert) bmOpCounts percentCounts 
                                                :: [[AnalysisData]]
  bmNames       = map bmName  counts            :: [String]
  bmLabels      = map bmLabel counts            :: [ProgramClass]
  percentCounts = map percentsOfTotal rawCounts :: [[Double]]
  rawCounts     = map (map snd) bmOpCounts      :: [[PinCounter]]
  bmOpCounts    = map pinData counts            -- :: [[(k,PinCounter)]]
  convert (code, count) percent = 
      AnalysisData { 
          label        = mkLabel code
        , rawCount     = count
        , percentTotal = percent
      }

percentsOfTotal :: [PinCounter] -> [Double]
percentsOfTotal counts = map ((/total).fromIntegral) counts
    where total = fromIntegral (sum counts)

dropUnimportantData :: Double -> [PinAnalysisData] ->  [PinAnalysisData] 
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


chooseImportantOpCodes :: Double -> PinAnalysisData -> Set AnalysisLabel
chooseImportantOpCodes threshold  d =
  foldr unionIf (Set.empty) (pinData d)
  where
  unionIf aData set =
    if (percentTotal aData) > threshold 
    then Set.insert (label aData) set 
    else set


module PinData where
import ClusterElement
import Data.Function
import Data.List
import Data.Set(Set) 
import qualified Data.Set as Set
import qualified Data.Map as Map
import Opcodes
import Jumpcodes
import Regcodes
import Papicodes
import OpcodeType
import OpcodeGroup

data GenPinData a = PinData { 
      bmName  :: String
    , bmLabel :: ProgramClass
    , pinData :: a
} deriving(Show, Read)

data PinTool = OpcodeMix | JumpMix | RegMix | BBLengthMix | PapiMix
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
  | PapiLabel     PapiEvent
  | OpcodeTypeLabel OpcodeType
    deriving (Eq, Show, Read, Ord)

alEnum :: AnalysisLabel -> Int
alEnum (OpcodeLabel oc)   = fromEnum oc
alEnum (JumpLabel   jl)   = fromEnum jl
alEnum (RegLabel    rl)   = fromEnum rl
alEnum (BBLengthLabel ll) =          ll
alEnum (PapiLabel     pl) = fromEnum pl
alEnum (OpcodeTypeLabel oc) = fromEnum oc

convertToAnalysisData :: 
     [GenCountData k] 
  -> (k -> AnalysisLabel)
  -> [PinAnalysisData]
convertToAnalysisData counts mkLabel =
  zipWith3 PinData bmNames bmLabels analysisData
  where
  analysisData  = zipWith (zipWith convert) bmOpCounts percentCounts 
                                                :: [[AnalysisData]]
  bmNames       = map bmName  counts            :: [String]
  bmLabels      = map bmLabel counts            :: [ProgramClass]
  percentCounts = map percents bmOpCounts       :: [[Double]]
  bmOpCounts    = map pinData counts            -- :: [[(k,PinCounter)]]
  percents      = computePercents mkLabel
  convert (code, count) percent = 
      AnalysisData { 
          label        = mkLabel code
        , rawCount     = count
        , percentTotal = percent
      }


computePercents :: (k -> AnalysisLabel) -> [(k, PinCounter)] -> [Double]
computePercents _mkLabel []   = []
computePercents  mkLabel bmOpCount@(x:_) = 
  ifPapiThen x 
    (\_ -> papiPercents papiData) 
    (percentsOfTotal (map snd bmOpCount))
  where
  ifPapiThen cnt papiCont nonPapiCont =
    case mkLabel (fst cnt) of 
      PapiLabel pl -> papiCont (pl, snd cnt)
      _            -> nonPapiCont 
  papiData = map extractPapiData bmOpCount
  extractPapiData cnt = ifPapiThen cnt id (error "Expected Papi label")

papiPercents :: [(PapiEvent, PinCounter)] -> [Double]
papiPercents counts = map percent counts
  where 
  percent (PAPI_L2_DCH, _count) = papiL2DchPercent
  percent (event, count) =
    case find (\x -> papiNormalizer event == fst x) counts of
      Just (_,norm) -> (fromIntegral count) / (fromIntegral norm)
      Nothing       -> 0.0
  papiL2DchPercent = 1.0 - papiL2DcmPercent
  papiL2DcmPercent = 
    case find (\(e,_) -> e == PAPI_L2_DCM) counts of
      Just dcm -> percent dcm
      Nothing  -> 0.0
    

percentsOfTotal :: [PinCounter] -> [Double]
percentsOfTotal counts = map ((/total).fromIntegral) counts
    where total = fromIntegral (sum counts)

fillMissingData :: Ord k => [GenCountData k] -> [GenCountData k]
fillMissingData allPinData = zipWith fill allPinData mapData
    where
    opcodes = collectAllKeys allPinData
    mapData = map (\d -> Map.fromListWith (+) (pinData d)) allPinData
    fill d md = d {pinData = map (countOrZero md) opcodes}
    countOrZero opMap opcode = 
        case Map.lookup opcode opMap of
            Just count -> (opcode, count)
            Nothing    -> (opcode, 0)

collectAllKeys :: Ord k => [GenCountData k] -> [k]
collectAllKeys allPinData = Set.toList . Set.fromList $ allOpCodes
    where 
    allOpCodes = map fst (concatMap pinData allPinData)

dropUnimportantData :: Double -> [PinAnalysisData] ->  [PinAnalysisData] 
dropUnimportantData threshold analysisData = 
  map filterData analysisData
  where
  filterData opcodeData =
    let opcodeFilter = (\aData -> Set.member (label aData) chosenOnes) in
    opcodeData {
        pinData = filter opcodeFilter (pinData opcodeData)
    }
    
  sets       = map (chooseImportantOpCodes threshold) analysisData
  threshSet  = foldr Set.union Set.empty sets :: Set AnalysisLabel
  chosenOnes = applyExtraFilters threshSet


chooseImportantOpCodes :: Double -> PinAnalysisData -> Set AnalysisLabel
chooseImportantOpCodes threshold  d =
  foldr unionIf (Set.empty) (pinData d)
  where
  unionIf aData set =
    if (percentTotal aData) > threshold 
    then Set.insert (label aData) set 
    else set

applyExtraFilters :: Set AnalysisLabel -> Set AnalysisLabel
applyExtraFilters set = Set.filter filterFun set
  where 
  filterFun (PapiLabel pl) = papiOutputFilter pl
  --filterFun (OpcodeLabel ol) = opcodeOutputFilter ol
  filterFun  _             = True

opcodeOutputFilter :: Opcode -> Bool
opcodeOutputFilter = opcodeFilterForGroup Other
  
-- |Summarize the data, first grouping by 'ProgramClass'.
-- the summarized data is returned as pin data with a single entry for each
-- program class
summarizeData :: [PinAnalysisData] -> [PinAnalysisData]
summarizeData pdata = 
  zipWith zipFun byProgramClass summedData 
  where
  zipFun pd ad = repr { bmName  = show (bmLabel repr)
                      , bmLabel = bmLabel repr
                      , pinData = (map (divide (length pd)) ad) }
                     where repr = head pd
  divide n ad = ad {  rawCount = (rawCount ad) `div` (fromIntegral n)
                    , percentTotal = (exp divided)}
                    where divided  = ((percentTotal ad) / (fromIntegral n))
  summedData = map -- for each program class
              (map -- for each analysis label
              (foldl1' total . map logIt ))
              byAnalysisLabel           :: [[AnalysisData]]
  byAnalysisLabel = map groupByAnalysisLabel byProgramClass 
  byProgramClass  = groupByProgramClass pdata :: [[PinAnalysisData]]
  total adata adataSum =
    adataSum {
        rawCount     = (rawCount adataSum)     + (rawCount adata)
      , percentTotal = (percentTotal adataSum) + (percentTotal adata)
    }
  logIt adata = 
    -- change percent value to log space
    -- setting anything <= 0 to a small number before hand
    let p = percentTotal adata 
        pValue = if p <= 0.0 then 1e-12 else p
    in
    adata { percentTotal = log pValue }


-- |Group the pin data by its program class
groupByProgramClass :: [PinAnalysisData] -> [[PinAnalysisData]]
groupByProgramClass = partitionListBy compareLabels
  where
  compareLabels = compare `on` bmLabel

-- |Group analysis data with a list for each different analysis label
groupByAnalysisLabel :: [PinAnalysisData]  -- one entry for each benchmark
                     -> [[AnalysisData]]   -- one entry for each ALabel
groupByAnalysisLabel = 
  (partitionListBy (compare `on` label)) . (concatMap pinData)

partitionListBy :: (a -> a -> Ordering) -> [a] -> [[a]]
partitionListBy compareFun list = 
  groupBy sameData (sortBy compareFun list)
  where
  sameData a b = EQ == (compareFun a b)




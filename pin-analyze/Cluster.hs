module Cluster where
import ClusterElement
import PinData
import Data.List
import qualified Data.IntMap as IntMap
import KMeans
import System.IO
import System.Random

type PinClusterElement = ClusterElement AnalysisLabel
type PinCluster        = [PinClusterElement]

clusterK :: (RandomGen g) => g ->  [PinAnalysisData] -> Int -> [PinCluster]
clusterK gen analysisData numClusters = kmeans gen numClusters clusterElements
  where
  clusterElements = convertToClusterElements analysisData

convertToClusterElements :: [PinAnalysisData] -> [PinClusterElement]
convertToClusterElements analysisData = map convert analysisData
  where 
  convert e = CE {
      shortName = bmName  e
    , dataLabel = bmLabel e
    , dataPoint = map convertPoint (pinData e)
  }
  convertPoint (AnalysisData l _ p) = (l,p)
        

writeClusters :: Handle -> [PinCluster] -> IO ()
writeClusters h clusters = 
  mapM_ printCluster $ zip (clusterStats clusters) clusters
  where
  totalCount = show (length (concat clusters))
  --printCluster :: ((Int, [(ProgramClass, Int)]),[OpcodeClusterElement]) -> IO ()
  printCluster ((i, classCounts), clusterElems) = do
    let line  = concatMap ((++ " ").showElement) clusterElems
    let count = show (length clusterElems)
    hPutStr   h (show i)
    hPutStrLn h $ " ("++count++"/"++totalCount++")"
    --hPutStr   h "[ "
    hPutStrLn h (show classCounts)--(concatMap ((++" ").show) classCounts)
    --hPutStrLn h "]"
    hPutStrLn h line

  showElement :: PinClusterElement -> String
  showElement  e = shortName e

  --showClass (pc, cnt) = (show pc) ++ ":"++(show cnt)
    
    

clusterStats :: [PinCluster] -> [(Int, [(ProgramClass, Int)])]
clusterStats clusters = stats
  where
  stats    = zip [1..] (map occurs clusters)
  occurs c = enum $ IntMap.toList $ foldr countOccurs (IntMap.empty) c
  enum   m = map (\(pc,cnt) -> (toEnum pc, cnt)) m
  countOccurs clusterElement occursMap = 
    let keyVal = fromEnum (dataLabel clusterElement) 
        count  = IntMap.findWithDefault 0 keyVal occursMap
    in
    IntMap.insert keyVal (count + 1) occursMap




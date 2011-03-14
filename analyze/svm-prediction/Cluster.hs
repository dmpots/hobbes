module Cluster where
import ClusterElement
import PinData
import Data.List
import qualified Data.IntMap as IntMap
import KMeans
import Numeric
import System.IO
import System.Random
import Util

type PinClusterElement = ClusterElement AnalysisLabel
type PinCluster        = [PinClusterElement]

clusterK :: (RandomGen g) => g ->  [PinAnalysisData] -> Int -> ([PinCluster],g)
clusterK gen analysisData numClusters = 
  kmeans gen numClusters clusterElements
  where
  clusterElements = convertToClusterElements analysisData

--clusterAccuracy :: [PinAnalysisData] -> [PinCluster] -> Double
kMeansRandIndex :: [GenPinData a] -> [GenCluster b c] -> Double
kMeansRandIndex candidates clusters = randIndex candidates cluster1 cluster2
  where
  cluster1 = clusterAssignment clusters
  cluster2 = bmLabel

-- Compute the Rand Index of the cluster assignments
-- 
-- Given a set of n elements  and two partitions of S to compare,  X and Y we
-- define the following:
-- a, the number of pairs of elements in S that are in the same set in X and in
--    the same set in Y
-- b, the number of pairs of elements in S that are in different sets in X and
--    in different sets in Y
-- c, the number of pairs of elements in S that are in the same set in X and in
--    different sets in Y
-- d, the number of pairs of elements in S that are in different sets in X and
--    in the same set in Y
-- 
-- The rand index R is:
-- R = a + b / (a + b + c + d) = a + b / (n choose 2)
randIndex :: Eq b => Eq c => [a] -> (a -> b) -> (a -> c) -> Double
randIndex candidates partition1 partition2 = (a + b) / nc2
  where
  a  = toFloat as
  b  = toFloat bs
  nc2= toFloat allPairs
  as = [bm1 | (bm1,bm2) <- allPairs, 
                           (partition1 bm1 == partition1 bm2), 
                           (partition2 bm1 == partition2 bm2)]
  bs = [bm1 | (bm1,bm2) <- allPairs, 
                           (partition1 bm1 /= partition1 bm2), 
                           (partition2 bm1 /= partition2 bm2)]
  allPairs = pairs candidates
  toFloat  = (fromIntegral . length)

clusterAssignment :: [GenCluster a b] -> GenPinData c -> Integer
clusterAssignment clusters pinAnalData = 
  case found of 
      Nothing    -> error("No cluster assignment found for pin data: "++benchName)
      Just (_,n) -> n
  where
  found = find (\(ces, _) -> any (\ce -> (shortName ce)== benchName) ces) cs
  cs    = zip clusters [1..]
  benchName = (bmName pinAnalData)

convertToClusterElements :: [PinAnalysisData] -> [PinClusterElement]
convertToClusterElements analysisData = map convert analysisData
  where 
  convert e = CE {
      shortName = bmName  e
    , dataLabel = bmLabel e
    , dataPoint = map convertPoint (pinData e)
  }
  convertPoint (AnalysisData l _ p) = (l,p)
        

writeClusters :: Handle -> [PinCluster] -> [PinAnalysisData] -> IO ()
writeClusters h clusters filteredResults = do
  mapM_ printCluster $ zip (clusterStats clusters) clusters
  putStrLn ("Accuracy = " ++ accuracyS ++ "%")
  --putStrLn ("STATS(min/mean/max) = " ++ 
  --          accuracyS ++ "/" ++ accuracyS ++ "/"++accuracyS )
  return ()
  where
  accuracyS  = (showFFloat (Just 4) accuracy "")
  accuracy   = (kMeansRandIndex filteredResults clusters) * 100.0
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




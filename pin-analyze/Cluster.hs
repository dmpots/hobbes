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
randIndex :: [PinAnalysisData] -> [PinCluster] -> Double
randIndex candidates clusters = (a + b) / nc2
  where
  a  = toFloat as
  b  = toFloat bs
  nc2= toFloat allPairs
  as = [(assign bm1) | (bm1,bm2) <- allPairs, 
                                    (assign bm1 == assign bm2), 
                                    (bmLabel bm1 == bmLabel bm2)]
  bs = [(assign bm1) | (bm1,bm2) <- allPairs, 
                                    (assign bm1 /= assign bm2), 
                                    (bmLabel bm1 /= bmLabel bm2)]
  allPairs = pairs candidates
  assign   = clusterAssignment clusters
  toFloat  = (fromIntegral . length)

--clusterAssignments :: [String] -> [PinCluster] -> IntMap
--clusterAssignments bmNames clusters =
--  foldr foldFun 
--  where
--  foldFun = (\bmName bmMap -> 
--              IntMap.insert bmName (clusterAssignment clusters bmName))

clusterAssignment :: [PinCluster] -> PinAnalysisData -> Integer
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
  accuracy   = (randIndex filteredResults clusters) * 100.0
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




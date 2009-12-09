module Cluster where
import ClusterElement
import Data.List
import KMeans
import Opcodes
import OpcodeMix
import System.IO

type OpcodeClusterElement = ClusterElement Opcode
type OpcodeCluster        = [ClusterElement Opcode]

cluster :: [PinOpCodeAnalysisData] -> Int -> [OpcodeCluster]
cluster analysisData numClusters = kmeans numClusters clusterElements
  where
  clusterElements = convertToClusterElements analysisData

convertToClusterElements :: [PinOpCodeAnalysisData] -> [OpcodeClusterElement]
convertToClusterElements analysisData = map convert analysisData
  where 
  convert e = CE {
      shortName = bmName e
    , dataLabel = Unknown
    , dataPoint = map convertPoint (opCounts e)
  }
  convertPoint (AnalysisData (OpcodeLabel oc) _ p) = (oc,p)
        

writeClusters :: Handle -> [OpcodeCluster] -> IO ()
writeClusters h clusters = mapM_ printCluster (zip nums clusters)
  where
  nums  = [show i | i <- [1..(length clusters)]]

  printCluster :: (String, [OpcodeClusterElement]) -> IO ()
  printCluster (i, clusterElems) = do
    let line  = concatMap ((++ " ").showElement) clusterElems
    hPutStr   h i 
    hPutStr   h $ ":("++(show (length clusterElems))++"): "
    hPutStrLn h line

  showElement :: OpcodeClusterElement -> String
  showElement (CE n l dp) = n 
    




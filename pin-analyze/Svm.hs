module Svm where

import Cluster
import ClusterElement
import Data.List
import Data.SVM
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import System.IO

---
---
---
v1 = IntMap.fromList [(1, 0.25), (2, 0.25)]
v2 = IntMap.fromList [(1, 0.5),  (2, 0.75)]
v3 = IntMap.fromList [(1, 0.5),  (2, 0.5)]

p  = [(1.0, v1), (2.0, v2), (2.0, v3), (1.0, v1)]
t  = train (CSvc 1.0) (RBF 1.0) p
d  = t >>= (\m -> return $ predict m v3)
cc  = crossValidate (CSvc 1.0) (RBF 1.0) p 2
  

trainModel' :: Double -> Double -> [OpcodeClusterElement] -> IO Model
trainModel' c gamma vectors = train algorithm kernel problem
  where
      algorithm = CSvc c
      kernel    = RBF gamma
      problem   = undefined


writeSVMFormattedData :: Handle -> [OpcodeClusterElement] -> IO ()
writeSVMFormattedData h clusterData = mapM_ (printSVM h) clusterData
  where
  printSVM h element = 
    let label  = show  (fromEnum . dataLabel $ element)  ++ " "
        points = (concat . (intersperse " ") . map format) (dataPoint element)
        format = (\(op, percent) -> (show . fromEnum) op++":"++show percent)
    in
    hPutStr h label >> hPutStrLn h points 


    




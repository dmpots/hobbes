module Svm where

import Cluster
import ClusterElement
import Data.List
import Data.SVM
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import System.IO

{-
v1 = IntMap.fromList [(1, 0.25), (2, 0.25)]
v2 = IntMap.fromList [(1, 0.5),  (2, 0.75)]
v3 = IntMap.fromList [(1, 0.5),  (2, 0.5)]

p  = [(1.0, v1), (2.0, v2), (2.0, v3), (1.0, v1)]
t  = train (CSvc 1.0) (RBF 1.0) p
d  = t >>= (\m -> return $ predict m v3)
cc  = crossValidate (CSvc 1.0) (RBF 1.0) p 2
-}
type PredictionElement = (String, ProgramClass, Vector)
type PredictedElement  = (String, ProgramClass, ProgramClass)
  

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


writePredictionDataUsingModel::FilePath->Handle->[OpcodeClusterElement]->IO ()
writePredictionDataUsingModel modelFile h elements = do
  model <- loadModel modelFile
  let predicted = predictElements model elements
  hPutStrLn h (formatPredictions predicted) 

formatPredictions :: [PredictedElement] -> String
formatPredictions predictions = 
  header ++ "\n~~~Correct~~~\n" ++ rightList ++ "\n~~~Incorrect~~~\n" ++ wrongList
  where
  (right, wrong) = partition correctPrediction predictions
  correctPrediction (_, a, b) = a == b
  header = "Correctly predicted " ++(show.length $ right)
           ++" out of " ++(show.length $ predictions)
           ++" ("++(show formatPercent)++")"
  formatPercent = (fromIntegral (length right) / fromIntegral (length predictions))
  rightList =  formatList right
  wrongList = formatList wrong
  formatList = (concat . (intersperse "\n") . (map formatPrediction))
  formatPrediction (n,actual,predicted) =
     n ++ " [Acutal = "++(show actual)++", Predicted = "++(show predicted)++"]"

predictElements :: Model -> [OpcodeClusterElement] -> [PredictedElement]
predictElements model elements = predictIt converted
  where
  predictIt = map (\(name, klass, vec) -> (name, klass, predictVector model vec))
  converted = convertElementsToVectors elements

convertElementsToVectors :: [OpcodeClusterElement] -> [PredictionElement]
convertElementsToVectors = map convertElement 

convertElement :: OpcodeClusterElement -> PredictionElement
convertElement element =
  (shortName element, dataLabel element, IntMap.fromList vecList)
  where
  vecList = map (\(x, y) -> (fromEnum x, y)) (dataPoint element)

predictVector :: Model -> Vector -> ProgramClass
predictVector model vector = prediction
  where
  prediction = enumVal $ predict model vector
  enumVal    = toEnum . fst . properFraction
  


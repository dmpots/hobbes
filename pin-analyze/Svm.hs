module Svm where

import Cluster
import ClusterElement
import Data.List
import Data.SVM
import qualified Data.IntMap as IntMap
import PinData
import System.IO

type PredictionElement = (String, ProgramClass, Vector)
type PredictedElement  = (String, ProgramClass, ProgramClass)
  
writeSVMFormattedData :: Handle -> [PinClusterElement] -> IO ()
writeSVMFormattedData handle clusterData = mapM_ (printSVM handle) clusterData
  where
  printSVM h element = 
    let classLabel  = show  (fromEnum . dataLabel $ element)  ++ " "
        points = (concat . (intersperse " ") . map format) (dataPoint element)
        format = (\(op, percent) -> (show.alEnum$op)++":"++show percent)
    in
    hPutStr h classLabel >> hPutStrLn h points 


writePredictionDataUsingModel::FilePath->Handle->[PinClusterElement]->IO ()
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
  formatPercent = (fromIntegral (length right) / totalPreds)
  totalPreds = fromIntegral (length predictions) :: Double
  rightList =  formatList right
  wrongList = formatList wrong
  formatList = (concat . (intersperse "\n") . (map formatPrediction))
  formatPrediction (n,actual,predicted) =
     n ++ " [Acutal = "++(show actual)++", Predicted = "++(show predicted)++"]"

predictElements :: Model -> [PinClusterElement] -> [PredictedElement]
predictElements model elements = predictIt converted
  where
  predictIt = map (\(name, klass, vec) -> (name, klass, predictVector model vec))
  converted = convertElementsToVectors elements

convertElementsToVectors :: [PinClusterElement] -> [PredictionElement]
convertElementsToVectors = map convertElement 

convertElement :: PinClusterElement -> PredictionElement
convertElement element =
  (shortName element, dataLabel element, IntMap.fromList vecList)
  where
  vecList = map (\(x, y) -> (alEnum x, y)) (dataPoint element)

predictVector :: Model -> Vector -> ProgramClass
predictVector model vector = prediction
  where
  prediction = enumVal $ predict model vector
  enumVal    = toEnum . fst . properFraction
  


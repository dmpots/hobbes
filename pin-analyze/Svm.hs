module Svm where

import Control.Monad
import Cluster
import ClusterElement
import Data.List
--import Data.Datamining.Classification.LibSVM
import Data.SVM
import qualified Data.IntMap as IntMap
import PinData
import Numeric
import System.IO
import System.Exit
import System.Process
import Text.Printf

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
  predictH <- openFile predictionFile WriteMode
  writeSVMFormattedData predictH elements
  hClose predictH
  systemWithCheck cmd
  results <- lines `liftM` readFile outFile
  let predicted = zipWith zipF elements results
  hPutStrLn h (formatPredictions predicted) 
  where
  cmd = "./libsvm-2.9/svm-predict "++predictionFile++" "++modelFile++" "++outFile
  predictionFile = "TRAIN.svm"
  outFile = "PREDICT.out"
  zipF element prediction = 
    (shortName element, dataLabel element, toEnum (read prediction))

formatPredictions :: [PredictedElement] -> String
formatPredictions predictions = 
  header ++ "\n~~~Correct~~~\n" ++ rightList ++ "\n~~~Incorrect~~~\n" ++ wrongList
  where
  (right, wrong) = partition correctPrediction predictions
  correctPrediction (_, a, b) = a == b
  header =    "Precision = "++ formatAccuracy
           ++ " ("++(show.length $ right)++"/"++(show.length $ predictions)++")"
           ++ "\n"
           ++ "RandIndx = "++formatRandIndex
  formatAccuracy   = (showFFloat (Just 4) accuracy  "%")
  formatRandIndex  = (showFFloat (Just 4) randIdx   "%")
  accuracy = (fromIntegral (length right) / totalPreds) * 100.0
  randIdx  = (svmRandIndex predictions) * 100.0
  totalPreds = fromIntegral (length predictions) :: Double
  rightList =  formatList right
  wrongList = formatList wrong
  formatList = (concat . (intersperse "\n") . (map formatPrediction))
  formatPrediction (n,actual,predicted) =
     n ++ " [Acutal = "++(show actual)++", Predicted = "++(show predicted)++"]"

writePredictionDataUsingFFI::FilePath->Handle->[PinClusterElement]->IO ()
writePredictionDataUsingFFI modelFile h elements = do
  model <- loadModel modelFile
  let predicted = predictElements model elements
  hPutStrLn h (formatPredictions predicted) 

predictElements :: Model -> [PinClusterElement] -> [PredictedElement]
predictElements model elements = predictIt converted
  where
  predictIt = map (\(name, klass, vec) -> (name, klass, predictVector model vec))
  --predictIt = mapM (\(name, klass, vec) -> predictVector model vec >>= \p -> return (name, klass, p))
  converted = convertElementsToVectors elements

convertElementsToVectors :: [PinClusterElement] -> [PredictionElement]
convertElementsToVectors = map convertElement 

convertElement :: PinClusterElement -> PredictionElement
convertElement element =
  (shortName element, dataLabel element, IntMap.fromList vecList)
  --(shortName element, dataLabel element, vecList)
  where
  vecList = map (\(x, y) -> (alEnum x, y)) (dataPoint element)
  --vecList = map snd (dataPoint element)

predictVector :: Model -> Vector -> ProgramClass
predictVector model vector = 
  let prediction = predict model vector in
  enumVal prediction
  where
  enumVal    = toEnum . fst . properFraction
  

svmRandIndex :: [PredictedElement] -> Double
svmRandIndex predicted = randIndex predicted partition1 partition2
  where
  partition1 = predictedClass
  partition2 = actualClass
  predictedClass (_, _, p) = p
  actualClass    (_, a, _) = a


systemWithCheck :: String -> IO ()
systemWithCheck cmd 
  = do
      ec <- system cmd 
      case ec of
        ExitSuccess      -> return ()
        ExitFailure code -> printf "execution failed (exit %d)\n" code >> 
                            fail "Execute command failed"



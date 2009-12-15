module KMeans (kmeans, kmeans')
    where

import ClusterElement
import Data.List (transpose, sort, groupBy, minimumBy, sortBy)
import Data.Function (on)
import Data.Ord (comparing)
import System.Random

type NPoint   = [Double]
type Centroid = NPoint
type Vector b = [(b, Double)]

dist :: ClusterElement b -> NPoint -> Double
dist ce point = sqrt . sum $ zipWith (\x y-> (x-y) ^ 2) cePoint point
  where
  cePoint = map snd (dataPoint ce)

centroid :: Cluster b -> NPoint
centroid cluster = map (flip (/) l . sum) $ transpose points
    where l = fromIntegral $ length points
          dp = (map dataPoint cluster) -- :: [Vector b a]
          points = map (map snd) dp

closest :: [Centroid] -> ClusterElement b -> NPoint
closest points point = 
  minimumBy (comparing $ dist point) points

recluster' :: [Centroid] -> [ClusterElement b] -> [Cluster b]
recluster' centroids clusters = map (map snd) $ groupBy clusterEq reclustered
    where 
    reclustered = sortBy clusterOrd [(closest centroids a, a) | a <- clusters]
    clusterEq  = ((==) `on` fst) 
    clusterOrd = comparing fst
    
recluster :: [Cluster b] -> [Cluster b]
recluster clusters = recluster' centroids $ concat clusters
    where centroids = map centroid clusters

part :: Int -> Int -> [a] -> [[a]]
part n dim ys
     | n <= 1    = [zs]
     | otherwise = zs : part (n-1) dim zs'
    where (zs, zs') = splitAt dim ys

-- | Recluster points
--kmeans' :: (Num a, Ord a) => [[Vector a]] -> [[Vector a]]
kmeans' :: (Eq b) => [Cluster b] -> [Cluster b]
kmeans' clusters
    | clusters == clusters' = clusters
    | otherwise             = kmeans' clusters'
    where clusters' = recluster clusters

-- | Cluster points into k clusters.
-- |
-- | The initial clusters are chosen arbitrarily
kmeans :: RandomGen g => (Eq b) => g -> Int -> [ClusterElement b] -> [Cluster b]
kmeans gen k []    =  []
kmeans gen k elems = 
  kmeans' initialClusters
  where 
  --initialClusters = recluster' initialPoints elems
  initialClusters = randomPartition gen k elems
  initialPoints   = part k (length (dataPoint . head $ elems)) randomSource
  randomSource    = randomRs (0, 1.0) gen :: [Double]

randomPartition :: RandomGen g => g -> Int -> [a] -> [[a]]
randomPartition gen k list = partition gen initial list
  where
  initial = replicate k []
  partition g partitions []     = partitions
  partition g partitions (x:xs) = 
    let (index, g') = randomR (0, k-1) g
        (yss, ys:yss') = splitAt index partitions
    in
    partition g' (yss ++ [x:ys] ++ yss') xs
  


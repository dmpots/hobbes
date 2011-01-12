module KMeans (kmeans)
    where

import ClusterElement
import Data.List (transpose, groupBy, minimumBy, sortBy)
import Data.Function (on)
import Data.Ord (comparing)
import System.Random
import Util

type NPoint   = [Double]
type Centroid = NPoint

dist :: ClusterElement b -> NPoint -> Double
dist ce point = sqrt . sum $ zipWith (\x y-> square (x-y)) cePoint point
  where
  square  x = x ^ (2 :: Integer)
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
kmeans :: RandomGen g 
          => (Eq b) 
          => g 
          -> Int 
          -> [ClusterElement b] 
          -> ([Cluster b], g)
kmeans gen _ []    =  ([], gen)
kmeans gen k elems = 
  (kmeans' initialClusters, gen')
  where 
  (initialClusters, gen') = randomPartition gen k elems



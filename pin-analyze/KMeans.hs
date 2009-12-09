module KMeans (kmeans, kmeans')
    where

import Data.List (transpose, sort, groupBy, minimumBy, sortBy)
import Data.Function (on)
import Data.Ord (comparing)
import ClusterElement

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

part :: Int -> [a] -> [[a]]
part x ys
     | null  zs' = [zs]
     | otherwise = zs : part x zs'
    where (zs, zs') = splitAt x ys

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
kmeans :: (Eq b) => Int -> [ClusterElement b] -> [Cluster b]
kmeans k elems = kmeans' $ part l elems
    where l = (length elems + k - 1) `div` k

module Data.KMeans (kmeans, kmeans')
    where

import Data.List (transpose, sort, groupBy, minimumBy)
import Data.Function (on)
import Data.Ord (comparing)

type Vector a = [a]

dist a b = sqrt . sum $ zipWith (\x y-> (x-y) ^ 2) a b

centroid points = map (flip (/) l . sum) $ transpose points
    where l = fromIntegral $ length points

closest points point = minimumBy (comparing $ dist point) points

recluster' centroids points = map (map snd) $ groupBy ((==) `on` fst) reclustered
    where reclustered = sort [(closest centroids a, a) | a <- points]

recluster clusters = recluster' centroids $ concat clusters
    where centroids = map centroid clusters

part x ys
     | zs' == [] = [zs]
     | otherwise = zs : part x zs'
    where (zs, zs') = splitAt x ys

-- | Recluster points
kmeans' :: (Floating a, Ord a) => [[Vector a]] -> [[Vector a]]
kmeans' clusters
    | clusters == clusters' = clusters
    | otherwise             = kmeans' clusters'
    where clusters' = recluster clusters

-- | Cluster points into k clusters.
-- |
-- | The initial clusters are chosen arbitrarily
kmeans :: (Floating a, Ord a) => Int -> [Vector a] -> [[Vector a]]
kmeans k points = kmeans' $ part l points
    where l = (length points + k - 1) `div` k

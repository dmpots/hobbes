module Util where
import System.Random

weave :: [a] -> [a] -> [a]
weave a b = concat $ zipWith (\x y -> x:y:[]) a b

evenPositions  :: [a] -> [a]
evenPositions  []    =     []
evenPositions (_:xs) =     unweaveHelper True xs

oddPositions   :: [a] -> [a]
oddPositions   []    =     []
oddPositions  (x:xs) = x : unweaveHelper False xs

unweaveHelper :: Bool -> [a] -> [a]
unweaveHelper _    [] = []
unweaveHelper True  (x:xs) = x : unweaveHelper False xs
unweaveHelper False (_:xs) =     unweaveHelper True  xs

randomPartition :: RandomGen g => g -> Int -> [a] -> ([[a]], g)
randomPartition gen k list = partition gen initial list
  where
  initial = replicate k []
  partition g partitions []     = (partitions, g)
  partition g partitions (x:xs) = 
    let (index, g') = randomR (0, k-1) g
        (yss, ys:yss') = splitAt index partitions
    in
    partition g' (yss ++ [x:ys] ++ yss') xs
  

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = [(x,y) | y <- xs] ++ pairs xs

{-
manyRandoms g k l 0   = []
manyRandoms g k l cnt = cs : (manyRandoms g' k l (cnt - 1))
  where
  (cs, g') = randomPartition g k l
-}

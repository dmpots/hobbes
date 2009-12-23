module Util where

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



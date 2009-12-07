module Util where

weave :: [a] -> [a] -> [a]
weave a b = concat $ zipWith (\x y -> x:y:[]) a b

unweave :: [a] -> [a]
unweave []           = []
evenPositions (x:xs) =     unweaveHelper True xs
oddPositions  (x:xs) = x : unweaveHelper False xs

unweaveHelper :: Bool -> [a] -> [a]
unweaveHelper _    [] = []
unweaveHelper True  (x:xs) = x : unweaveHelper False xs
unweaveHelper False (x:xs) =     unweaveHelper True  xs



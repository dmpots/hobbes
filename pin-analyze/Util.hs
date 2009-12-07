module Util where

weave :: [a] -> [a] -> [a]
weave a b = concat $ zipWith (\x y -> x:y:[]) a b


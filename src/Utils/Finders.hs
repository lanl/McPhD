module Utils.Finders where

import Data.List

-- A function to find the minimum element of a list and return it with it's index.
mindex :: (Ord a) => [a] -> (Int, a)
mindex [] = error "Empty list"
mindex x = mindex' 0 x
           where
             mindex' :: (Ord a) => Int -> [a] -> (Int, a)
             mindex' i (x:[]) = (i,x)
             mindex' i (x:xs) = minimumBy (\a b -> snd a `compare` snd b) [(i, x), mindex' (i+1) xs]


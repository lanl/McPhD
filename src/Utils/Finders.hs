module Utils.Finders where

import Data.List
import Data.Function

-- A function to find the minimum element of a list and return it with its index.
mindex :: (Ord a) => [a] -> (Int, a)
mindex = minimumBy (compare `on` snd) . zip [0..]

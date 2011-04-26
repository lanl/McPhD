module Search where

import Data.Vector as V

-- | Binary search in a sorted vector.
{-# INLINE binarySearch #-}
binarySearch :: (Ord a) => a -> Vector a -> Maybe Int
binarySearch = binarySearchBy compare

-- | Binary search in a sorted vector with a given comparison function.
{-# INLINE binarySearchBy #-}
binarySearchBy :: (a -> b -> Ordering) -> a -> Vector b -> Maybe Int
binarySearchBy cmp x xs = go 0 (V.length xs)
  where
    go :: Int -> Int -> Maybe Int
    go lo hi | lo >= hi  = Nothing
             | otherwise = let n = (lo + hi) `div` 2
                           in  case cmp x (xs ! n) of
                                 LT -> go lo      n
                                 EQ -> Just n
                                 GT -> go (n + 1) hi

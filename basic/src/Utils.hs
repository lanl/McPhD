-- Utils.hs
-- T. M. Kelley
-- Mar 27, 2012
-- (c) Copyright 2012 LANSLLC, all rights reserved

module Utils 
  -- ( chunkBy
  -- , cromulent )
  where

import Data.List 
import Data.Word

-- | chunk a list according to a list of lengths. The sum of the list of
-- lengths must equal the length of the list to be split. Hoogle doesn't find
-- a match for this (3/2012) ....
chunkBy :: Integral b => [b] -> [a] -> [[a]]
chunkBy ls xs = unfoldr go (ls,xs)
  where go ([],_) = Nothing
        go ([l],xs) = Just (xs,([],[]))
        go ( (l:ls),xs) = case splitAt (fromIntegral l) xs of 
                            ([],[]) -> Nothing
                            (ys,rs) -> Just (ys,(ls,rs))
{-# INLINE chunkBy #-}

cromulent :: [[Word32]] -> [[(a,Word32,c,d)]] -> Bool
cromulent idss sss = foldr (&&) True $ zipWith check idss sss
  where check ids ss = fromIntegral (length ids) == nParticles ss
        nParticles =  foldr (\(_,i,_,_) j -> i+j) 0

-- End of file

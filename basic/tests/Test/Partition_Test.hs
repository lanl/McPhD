-- Partition.hs
-- T. M. Kelley
-- Mar 22, 2012
-- (c) Copyright 2012 LANSLLC, all rights reserved

module Test.Partition_Test where

-- the module to test:
import Partition

-- Testing libraries
import Test.Framework (testGroup,Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Arbitraries
import Data.Word (Word32)
import Data.List (nub)

-- | For a given list of SrcStat's, the lists of particle ids per
-- rank, interleaved cell-by-cell, will be the same as the single
-- rank case. This is the most interesting property in these tests.
prop_genPtclIdsIndCommSz :: RankNComm -> SrcStatsV -> Bool
prop_genPtclIdsIndCommSz (RankNComm _ c) ssv@(SrcStatsV ss) =
  idsManyRanks c ssv == genPtclIds 0 1 ss

idsManyRanks :: Word32 -> SrcStatsV -> [Word32]
idsManyRanks c (SrcStatsV ss) = concat $ zipWith idsOneCell cumNs ss
  where cumNs = init $ scanl (\i (_,j,_,_) -> i+j) 0 ss
        idsOneCell cumN s = 
          concat $ map (\r -> idsPerCell r c cumN s) [0..(c-1)]

-- | ids are used only once, regardless of decomposition
-- For an arbitrary list of SrcStats, choose an arbitrary CommSz
-- for each cell and for each rank, check that each particle id is
-- used only once. 
prop_IdsUsedOnce :: RankNComm -> SrcStatsV -> Bool
prop_IdsUsedOnce (RankNComm _ csz) ssv =
  uniqueList $ allIds csz ssv

-- | assemble a list of particle ids for all ranks for all cells
allIds :: Word32 -> SrcStatsV -> [Word32]
allIds csz ssv = 
  concat [(idsPerCell r csz (cumN i) (ss !! (fromIntegral i))) | i<-is, r <- rs]
  where 
      is = [0..(nCells-1)] :: [Word32]
      rs = [0..(csz-1)] :: [Word32]
      ss = (unssV ssv)
      cumN j = cumNs !! (fromIntegral j :: Int)
      cumNs = init $ scanl (\j (_,i,_,_)->i+j) (0 :: Word32) ss
      nCells = fromIntegral $ length ss

-- | Each element of a list occurs only once
uniqueList :: Eq a => [a] -> Bool
uniqueList xs = xs == nub xs

-- | ids per rank per cell increase monotonically
prop_IdsPerCellMonotonic :: RankNCommNNC -> Bool
prop_IdsPerCellMonotonic (RankNCommNNC r c cumN s) = 
  monotonous $ idsPerCell r c cumN s
    where monotonous [] = True
          monotonous [_] = True
          monotonous (x:y:xs) = x <= y && monotonous (y:xs)

-- | there is one id per particle in one cell
prop_lenIdsPerCell :: RankNCommNNC -> Bool
prop_lenIdsPerCell (RankNCommNNC r c cumN s@(_,nc,_,_)) = 
  fromIntegral (psPerRank r c nc) == (length $ idsPerCell r c cumN s)

-- | there is one id per particle in all cells for a rank
prop_lenIds :: RankNComm -> SrcStatsV -> Bool
prop_lenIds (RankNComm r c) (SrcStatsV ss) = 
  (fromIntegral . sum $ map (\(_,nc,_,_) -> psPerRank r c nc) ss) == 
        (length $ genPtclIds r c ss)

tests :: [Test]
tests = 
  [ testGroup "Partition"
    [ 
      testProperty "genPtclIds ind. of rank & comm size" prop_genPtclIdsIndCommSz
    , testProperty "particle ids only used once" prop_IdsUsedOnce
    , testProperty "ids per cell per rank increase monotonically" prop_IdsPerCellMonotonic
    , testProperty "one id per particle (per cell)" prop_lenIdsPerCell
    , testProperty "one id per particle (global within rank)" prop_lenIds
    ]
  ]

-- version
-- $Id$

-- End of file

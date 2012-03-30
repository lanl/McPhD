-- Partition.hs
-- T. M. Kelley
-- Mar 22, 2012
-- (c) Copyright 2012 LANSLLC, all rights reserved

module Test.Partition_Test where

-- the module to test:
import Partition

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.Arbitraries

-- import Data.Word (Word32)

-- | For a given list of SrcStat's, the lists of particle ids per
-- rank, interleaved cell-by-cell, will be the same as the single
-- rank case. This is the most interesting property in these tests.
prop_genPtclIdsIndCommSz :: RankNComm -> SrcStatsV -> Bool
prop_genPtclIdsIndCommSz (RankNComm _ c) (SrcStatsV ss) =
  idsManyRanks == genPtclIds 0 1 ss
    where
      idsManyRanks = concat $ zipWith cellIds cumNs ss
      cumNs = init $ scanl (\i (_,j,_,_) -> i+j) 0 ss
      cellIds cumN s = 
        concat $ map (\r -> idsPerCell r c cumN s) [0..(c-1)]

-- | ids per rank per cell increase monotonically
prop_IdsPerCellMonotonic :: RankNCommNNC -> Bool
prop_IdsPerCellMonotonic (RankNCommNNC r c cumN s@(_,nc,_,_)) = 
  monotonous $ idsPerCell r c cumN s
    where monotonous [x] = True
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
  
tests = 
  [ testGroup "Partition"
    [ 
      testProperty "genPtclIds ind. of rank & comm size" prop_genPtclIdsIndCommSz
    , testProperty "ids per cell per rank increase monotonically" prop_IdsPerCellMonotonic
    , testProperty "one id per particle (per cell)" prop_lenIdsPerCell
    , testProperty "one id per particle (global within rank)" prop_lenIds
    ]
  ]

-- version
-- $Id$

-- End of file

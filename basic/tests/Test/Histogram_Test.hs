-- Histogram_Test.hs
-- T. M. Kelley
-- Sep 02, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Test.Histogram_Test (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck
import Test.Arbitraries

import Histogram
import Physical
import SoftEquiv

import qualified Data.Vector.Unboxed as U

-- * count

-- | count with a null weight should leave the counts & squares unchanged
prop_nullCountUnchanged :: HistNEnergy -> Bool
prop_nullCountUnchanged (HistNEnergy h e) = cmpEHists  h' h (EnergyWeight 1e-16)
  where h' = count h (e,EnergyWeight 0)

-- | count is commutative to within FP error (on same index)
--     (that is, {count h e1; count h e2} == {count h e2; count h e1})
prop_cmtvSameIdx :: HistNEnergy -> EnergyWeight -> EnergyWeight -> Bool
prop_cmtvSameIdx (HistNEnergy h e) ew1 ew2 = cmpEHists h1 h2 (EnergyWeight 1e-14)
  where h1 = count (count h (e,ew1)) (e,ew2)
        h2 = count (count h (e,ew2)) (e,ew1)

cmpEHists ha hb tol = U.foldl1' (&&) compCounts
  where compCounts  = U.zipWith cmpfunc (ehcounts ha) (ehcounts hb)
        compSquares = U.zipWith cmpfunc (ehsquares ha) (ehsquares hb)
        cmpfunc x y = softEquiv x y tol

-- | count is absolutely commutative (on different indices)


-- findBin properties
--   bb ! (findBin e bb) <= e
prop_fbLowerBound (HistNEnergy (EHist _ _ _ bbs) e) = bbs U.! i <= e
  where i = findBin e bbs

--   bb ! (findBin e bb) + 1 > e
prop_fbUpperBound (HistNEnergy (EHist _ _ _ bbs) e) = bbs U.! (i + 1) > e
  where i = findBin e bbs

--   throws exception if asked for an energy outside of the bin boundaries

-- divIfne0
prop_divIfne0_finite :: Double -> Double -> Bool
prop_divIfne0_finite x y = q /= 1/0 && q /= -1/0
  where q = x `divIfne0` y


-- aggregate tests for framework
tests = [testGroup "histogram tests"
         [
          testProperty "count 0 wt: histogram unchanged" prop_nullCountUnchanged
         ,testProperty "count commutes (within FP on same idx)" prop_cmtvSameIdx
         -- ,testProperty "count commutes (absolutely on different idx)" prop_cmtvDiffIdx
         ,testProperty "findBin is lower bound on e" prop_fbLowerBound
         ,testProperty "findBin + 1 is upper bound on e" prop_fbLowerBound
         ]
        ]


-- version
-- $Id$

-- End of file

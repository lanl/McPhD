-- | A test module for Sphere1D
module Test.Mesh_test (tests) where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck
import Test.Arbitraries

-- The library under test
import Sphere1D
import Mesh
import Cell
import Physical
import PRNG

import Debug.Trace


-- * samplePositionInCell: (1) pos >= lower boundary, 
-- (2) pos <= upper boundary
psnGELowBnd :: Sphere1D -> Cell -> Rnd Bool
psnGELowBnd s c = do
  p <- samplePositionInCell s c
  let lb = pos . lowB $ c
      x  = pos p
  return $ x >= lb

psnLEHighBnd :: Sphere1D -> Cell -> Rnd Bool
psnLEHighBnd s c = do
  p <- samplePositionInCell s c
  let hb = pos . highB $ c
      x  = pos p
  return $ x <= hb


tests = [testGroup "Sphere1D" 
         [ testProperty "samplePosInCell: x >= cell lower bound" psnGELowBnd
         , testProperty "samplePosInCell: x <= cell upper bound" psnLEHighBnd
         ]
        ]

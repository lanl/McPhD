-- | A test module for Sphere1D
module Test.Mesh_Test (tests) where

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

-- helpers
import Control.Applicative
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

-- * sampleDirection: direction cosine must be > -1.0 and < 1.0
dirBnded :: Sphere1D -> Rnd Bool
dirBnded s = do 
  (Direction o) <- sampleDirection s
  return $ -1.0 <= o && o <= 1.0

-- | distance to boundary >= 0. 
dToBoundGE0 :: Sphere1D -> PositionInCell -> Direction -> Bool
dToBoundGE0 s (PiC c p) o = d >= 0.0
  where (Distance d, _) = distanceToBoundary s c p o

tests = [testGroup "Sphere1D" 
         [ testProperty "samplePosInCell: x >= cell lower bound" psnGELowBnd
         , testProperty "samplePosInCell: x <= cell upper bound" psnLEHighBnd
         , testProperty "sampleDirection: -1 < omega < 1" dirBnded
         , testProperty "distanceToBoundary: d >= 0" dToBoundGE0
         ]
        ]

-- | A test module for Sphere1D
module Test.Sphere1D_test (tests) where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck
import Test.TestingTools

-- The library under test
import Sphere1D

-- It's dependencies
import Numerical
import Physical
import Data.Vector.V1
import Cell


import Data.List
import Data.Functor


-- Property: Positions sampled in a cell are in that cell.
prop_SamplesInCell :: FP -> FP -> FP -> Bool
prop_SamplesInCell a b c =
  let (position, cell) = sampPos infMesh a b c
  in (findCell infMesh $ v1x (pos position)) == cell


-- Property: Particle crosses inner surface when r*sin(phi) < -r_i and cos(phi) < 0.
prop_SurfaceCross :: Positive FP -> Positive FP -> Positive FP -> Unit -> Bool
prop_SurfaceCross (Positive r1) (Positive r2) (Positive r3) (Unit phi_interp) =
  let rmin : r : [rmax] = sort [r1, r2, r3]
      phi   = 2*pi*phi_interp -- Angle of particle motion, measured from positive r.
      omega = cos phi
      (_, face)  = distToBdy r (negate omega) rmax rmin
      innerCross = crossSphere rmin r phi
  in test face innerCross where
    test XLow  True  = True
    test XHigh False = True
    test _ _  = False


crossSphere :: FP -> FP -> FP -> Bool
crossSphere rmin r phi =
  let eta   = abs $ sin phi
      omega = cos phi
  in (r * eta < rmin) && (omega < 0)



tests = [ testGroup "Positions"
          [
            testProperty "samples in cell" prop_SamplesInCell
          ]
        , testGroup "Directions"
          [
            testProperty "correct face crossing" prop_SurfaceCross
          ]
        ]

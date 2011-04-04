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


-- Property: Face crossing is XLow iff ray crosses inner sphere.
prop_SurfaceCross :: Positive FP -> Positive FP -> Positive FP -> Unit FP -> Bool
prop_SurfaceCross (Positive r1) (Positive r2) (Positive r3) (Unit phi_interp) =
  let rmin : r : [rmax] = sort [r1, r2, r3]
      phi   = 2*pi*phi_interp -- Angle of particle motion, measured from positive r.
      omega = cos phi
      (_, face)  = distToBdy r (negate omega) rmax rmin  
      innerCross = crossSphere rmin r phi
  in case (face, innerCross) of
    (XLow,  True)  -> True
    (XHigh, False) -> True
    (_,_)          -> False

{-- A ray crosses a sphere it is outside of when:
    r_ray*|sin(phi)| < r_sphere, and 
    cos(phi) < 0.  
--}
crossSphere :: FP -> FP -> FP -> Bool
crossSphere r_sphere r_ray phi =
    let r_min = r_ray * (abs $ sin phi)
        omega = cos phi
  in (r_min < r_sphere) && (omega < 0)



tests = [ testGroup "Positions"
          [
            testProperty "samples in cell" prop_SamplesInCell
          ]
        , testGroup "Directions"
          [
            testProperty "correct face crossing" prop_SurfaceCross
          ]
        ]

-- | A test module for Sphere1D
module Test.Sphere1D_test (tests) where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

-- The library under test
import Sphere1D

-- It's dependencies
import Numerical
import Physical
import Data.Vector.V1
import Cell

-- Property: Positions sampled in a cell are in that cell. 
prop_SamplesInCell :: FP -> FP -> FP -> Bool
prop_SamplesInCell a b c = 
  let (position, cell) = sampPos infMesh a b c
  in (findCell infMesh $ v1x (pos position)) == cell 


newtype Unit = Unit FP  deriving (Show)
instance Arbitrary Unit where
    arbitrary = Unit $ choose (0.0, 1.0)


-- Property: Particle crosses inner surface when sin(phi) < -r_i / r. Outer otherwize
prop_SurfaceCross :: Positive FP -> Positive FP -> Unit -> FP -> Bool
prop_SurfaceCross (Positive r1) (Positive r2) (Unit r_interp) phi_interp =
  let rmin  = min r1 r2 -- Inner cell radius
      rmax  = max r1 r2 -- Outer cell radius
      r     = r1 + (r2-r1) * r_interp -- Particle position
      phi   = 2*pi*phi_interp -- Angle of particle motion, measured from positive r.
      eta   = abs $ sin phi  
      omega = cos phi 
      (_, face) = distToBdy r omega rmin rmax
  in test face r rmin eta omega where
    test XLow  r rmin eta omega = (eta < r/rmin)  && (omega < 0)
    test XHigh r rmin eta omega = (eta >= r/rmin) || (omega >= 0) 
    test _ _ _ _ _  = False
   


tests = [ testGroup "Positions"  [testProperty "samples in cell" prop_SamplesInCell]
        , testGroup "Directions" [testProperty "correct face crossing" prop_SurfaceCross]
        ]

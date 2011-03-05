-- | A test module for Sphere1D
module Test.Sphere1D_test (tests) where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit

-- The library under test
import Sphere1D

-- It's dependencies
import Numerical
import Physical
import Data.Vector.V1

-- Property: Positions sampled in a cell are in that cell. 
prop_SamplesInCell :: FP -> FP -> FP -> Bool
prop_SamplesInCell a b c = 
  let (position, cell) = sampPos infMesh a b c
  in (findCell infMesh $ v1x (pos position)) == cell 


tests = [testGroup "Positions" [testProperty "samples in cell" prop_SamplesInCell]]

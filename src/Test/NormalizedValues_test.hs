module Test.NormalizedValues_test where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

-- The library under test
import NormalizedValues

import Data.Vector.Class
import Approx
import Numerics
import NumericClasses
import Test.Numeric_arbitrary

-- A silly type, showing how to make Normable instances outside of
-- NormalizedValues.
newtype  Foo = Foo Double
instance Scale Foo where scale     (Foo f) s = Foo (f*s)
instance Mag   Foo where magnitude (Foo f)   = abs f

-- I make Foo an instance of Normable, getting a normalize function
-- without having access to the Normalized constructor.
instance Normable Foo

-- | Property: sampleNormalVector3 returns normalized direction vectors.
prop_UnitLength :: (UnitInterval Double) -> (UnitInterval Double) -> Bool
prop_UnitLength a b = let v = generateNormalVector3 a b in (vmag $ getValue v) ~== 1.0

tests = [ testGroup "Random Directions" [testProperty "unit length" prop_UnitLength]
        ]

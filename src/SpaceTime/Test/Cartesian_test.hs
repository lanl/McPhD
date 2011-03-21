module SpaceTime.Test.Cartesian_test where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

-- The library under test
import SpaceTime.Cartesian

-- Its dependencies
import SpaceTime.Classes
import Approx
import Data.Functor
import Control.Applicative
import Data.Vector.Class
import Data.Vector.V2

instance Arbitrary Vector2 where
  arbitrary = Vector2 <$> arbitrary <*> arbitrary

instance (BasicVector v) => Arbitrary (Cartesian v) where
  arbitrary = Cartesian v <$> arbitrary <*> arbitrary

-- Property: Moving no distance leaves location unchanged.
prop_ZeroDistance :: Cartesian Vector2 -> Bool
prop_ZeroDistance location = (location == stream location 0)

tests =
  [
    testGroup "Stream Operations"
    [
      testProperty "Zero distance -> Same location" prop_ZeroDistance
    ]
  ]
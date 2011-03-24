module SpaceTime.Test.Cart3D_test where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

-- The library under test
import SpaceTime.Cart3D

-- Its dependencies
import SpaceTime.Classes
import Approx
import Data.Functor
import Control.Applicative
import Data.Vector.V3

instance Arbitrary Vector3 where
  arbitrary = Vector3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Cart3D where
  arbitrary = Cart3D <$> arbitrary <*> arbitrary

-- Property: Moving no distance leaves location unchanged.
prop_ZeroDistance :: Cart3D -> Bool
prop_ZeroDistance location = (location == stream location 0)

tests =
  [
    testGroup "Stream Operations"
    [
      testProperty "Zero distance -> Same location" prop_ZeroDistance
    ]
  ]

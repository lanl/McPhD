module Test.Philox_Test where

-- The library under test
import Philo2
import Test.Arbitraries ()

-- Testing libraries
import Test.Framework (testGroup,Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

prop_URDsBounded :: RNG -> Bool
prop_URDsBounded g = 0 <= r1 &&
                     r1 <= 1 &&
                     0 <= r2 &&
                     r2 <= 1
  where (r1,r2) = random2 g

tests :: [Test]
tests = [ testGroup "Philox"
          [
           testProperty "random numbers >=0 amd <= 1" prop_URDsBounded
          ]
        ]

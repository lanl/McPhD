module Test.Arbitraries where

import Test.QuickCheck 
import Control.Monad (liftM)
import Control.Applicative ( (<$>) )
import Physical
import Constants

instance Arbitrary Energy where
  arbitrary = Energy <$> suchThat arbitrary (> 0.0)

instance Arbitrary Velocity where
  arbitrary = Velocity <$> choose (-c,c)

instance Arbitrary Direction where
  arbitrary = Direction <$> choose (-1.0,1.0)





module Test.Arbitraries where

import Test.QuickCheck 
import Control.Monad (liftM)
import Control.Applicative ( (<$>) )
import Physical
import Constants
import Cell
import Material
import Sigma_HBFC

-- QUESTION: how to use NonNegative here?

ge0 :: (Arbitrary a, Ord a, Fractional a) => Gen a
ge0 = suchThat arbitrary (>= 0.0)

-- Things naturally positive, or maybe 0
instance Arbitrary Energy where
  arbitrary = Energy <$> ge0

instance Arbitrary Temperature where
  arbitrary = Temperature <$> ge0

instance Arbitrary Density where
  arbitrary = Density <$> ge0

instance Arbitrary Opacity where 
  arbitrary = Opacity <$> ge0


instance Arbitrary Velocity where
  arbitrary = Velocity <$> choose (-c,c)

instance Arbitrary Direction where
  arbitrary = Direction <$> choose (-1.0,1.0)

instance Arbitrary Position where
  arbitrary = Position <$> arbitrary

instance Arbitrary Material where
  arbitrary = do
    sa <- arbitrary
    ss <- arbitrary
    v <- arbitrary
    t <- arbitrary
    rn <- arbitrary
    re <- arbitrary
    rp <- arbitrary
    return $ Material sa ss v t rn re rp
  
instance Arbitrary Cell where
  arbitrary = do
    lb <- arbitrary
    ub <- arbitrary
    lbc <- arbitrary
    ubc <- arbitrary
    mat <- arbitrary
    return $ Cell lb ub lbc ubc mat 

instance Arbitrary BoundaryCondition where
  arbitrary = oneof [return Vac, return Refl, return Transp] 

instance Arbitrary Lepton where
  arbitrary = oneof [return nu_e, return nu_e_bar, return nu_x, return nu_x_bar]

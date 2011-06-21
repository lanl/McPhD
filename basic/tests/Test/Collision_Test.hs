{- | Tests of Collision module -}

module Test.Collision_Test where

import Collision
import Test.Arbitraries ()
import SoftEquiv
import Physical

import Test.QuickCheck
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- * eventProbs should organize the probabilities of different collisions
-- into a list that is (1) monotonically increasing, (2) with max element 
-- approximately equal to 1.0, (3) with minimum element greater than 0
-- if nucleon density > 0.

-- | eventProbs (1)
prop_eventProbs_Monotonic :: Cell -> Energy -> Lepton -> Bool
prop_eventProbs_Monotonic c e sig = monotonic (eventProbs c e sig)
  where monotonic []       = True
        monotonic [_]      = True
        monotonic (x:y:xs) = x <= y && monotonic (y:xs)

-- | eventProbs (2) ... looks good to ~1e-15
prop_eventProbs_MaxLE1 :: Cell -> Energy -> Lepton -> Bool
prop_eventProbs_MaxLE1 c e sig =
  softEquiv (maximum $ eventProbs c e sig) 1.0 1e-15

-- | eventProbs (3) (condition on density satisfied in Arbitrary
prop_eventProbs_MinGT0 :: Cell -> Energy -> Lepton -> Bool
prop_eventProbs_MinGT0 c e sig = (minimum $ eventProbs c e sig) > 0.0 


-- * dCollideComoving should (1) be positive, (2) should be invertable in that
-- if we happen to sample a random number xi that equals exp(-mu), then d = 1.

-- | dCollideComoving (1)
prop_dCollCom_Pstv :: Cell -> Energy -> Lepton -> URD -> Bool
prop_dCollCom_Pstv c e sig xi = (distance $ dCollideComoving c e sig xi) > 0.0

{- | dCollideComoving (2)
  If xi = Exp[-mu], d = 1. This gives a nice test at the cost of constraining
  only one number. 

  Because of the tiny exponents (-44) in the neutrino cross sections,
  precision can get walloped. Thus the limits on xi.

  With the current limits on xi, we seem to stay well under the default
  limit of 500 discarded tests. Over about 1000 tests, might need to 
  raise the discard limit. These limits need to be thought through a bit
  further.
-}

-- prop_dCollCom_xiExpsig :: Cell -> Energy -> Lepton -> Property -- ?? not Bool?
prop_dCollCom_xiExpsig c e sig =
  xi > 0.000001 && xi < 0.999999 ==> softEquiv d 1.0 1e-5
    where xi = exp (- m)
          m  = mu $ opCollide c e sig
          d  = distance $ dCollideComoving c e sig (URD xi) 


tests = [testGroup "eventProbs"
         [
           testProperty  "monotonic" prop_eventProbs_Monotonic
         , testProperty  "max ~== 1" prop_eventProbs_MaxLE1
         , testProperty  "min > 0" prop_eventProbs_MinGT0
          ]
         , testGroup "dCollideMoving"
         [
           testProperty "positive" prop_dCollCom_Pstv
         , testProperty "xi=e(-mu) ==> d = 1" prop_dCollCom_xiExpsig
         ]

        ]


-- end of file

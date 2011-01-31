-- | A testing module for RandomStreaming
module Test.RandomStreaming_test (tests) where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- The library under test
import RandomStreaming

-- Its dependencies
import Particle.RandomParticle
import Particle.Test.Arbitrary ()
import Space3DCartesian
import Approx
import System.Random.Mersenne.Pure64
import Data.Vector.V3

origin :: Position
origin = Position (Vector3 0 0 0)

rand :: PureMT
rand = pureMT $ fromIntegral (0::Integer)


-- | The direction after the step should be either the same as before,
-- or differ by the scattering vector.
prop_StepMomentum :: RandomParticle -> Bool
prop_StepMomentum p = let next = step (Opacity 1.0) (Time 1.0) p in
  case next of
    -- | The momentum change is the difference between the initial final directions
    Just (Event _ (Scatter     d ), p') ->
      (mom d + (dir $ rpDir p)) ~== (dir $ rpDir p')

    -- | For other events, the direction is unchanged.
    Just (Event _ (FaceCrossing _), p') -> (rpDir p) ~== (rpDir p')
    Just (Event _ (Termination p'), _ ) -> (rpDir p) ~== (rpDir p')
    Just (Event _ (Escape      p'), _ ) -> (rpDir p) ~== (rpDir p')
    Nothing -> True

-- * Tests.  TODO: Needs more? Hard to test streaming results
-- operation without an operation to accumulate the tally.

-- | A regression test which happens to be seven steps long.
-- TODO: Add more checks on this sample stream.
-- sampleStream :: [Event]
-- sampleStream = (stream (Opacity 1.0) (Time 1.0) $
-- 		sampleIsoParticle rand origin (Speed 0.1) (Time 0.0) (Distance 5.0)
-- 		$ Local (CellIndex 0 0 0))

-- test_sampleStream :: Assertion
-- test_sampleStream = length sampleStream @?= 7

-- finalParticle :: RandomParticle
-- finalParticle = InFlight {
-- 		  rpPos = Position
-- 			  {
-- 			    pos = Vector3
-- 				  {
-- 				    v3x = -1.7679827673775232,
-- 				    v3y =  0.7502965565750843,
-- 				    v3z = -6.274089661889681e-2
-- 				  }
-- 			  }
-- 		, rpDir = direction_unsafe
-- 			  Vector3
-- 			  {
-- 			    v3x = -0.22920503548720722,
-- 			    v3y =  0.9565491573392524,
-- 			    v3z = -0.18021864859351883
-- 			  }
-- 		, rpSpeed = Speed { speed = 1.0 }
-- 		, rpTime = Time {time = 1.0}
-- 		, rpMFPToScatter = Distance {dis = 5.0}
-- 		, rpIndex = Local (CellIndex 0 0 0)
-- 		, rpRand = rand
--   }

get_final_particle :: Limiter -> Maybe RandomParticle
get_final_particle (Termination p) = Just p
get_final_particle (Escape p) = Just p
get_final_particle _ = Nothing


-- test_finalParticle :: Assertion
-- test_finalParticle = assertBool "final particle equality"
-- 		     ((get_final_particle $ eventLimit $ last sampleStream)
-- 		      ~== finalParticle)


tests =
  [
    testGroup "Step Operation"
    [
      testProperty "Momentum conservation" prop_StepMomentum
    ]
  , testGroup "Streaming Results"
    [
      -- testCase "Sample stream length" test_sampleStream,
      -- testCase "Sample stream final"  test_finalParticle
    ]
  ]

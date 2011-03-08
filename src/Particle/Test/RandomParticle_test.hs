-- | A testing module for RandomParticle
module Particle.Test.RandomParticle_test (tests) where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

-- The library under test
import Particle.RandomParticle

-- Its dependencies
import Space3DCartesian
import RandomValues
import Particle.Test.ArbitraryParticles
import Approx
import NumUnit
import System.Random.Mersenne.Pure64
import Data.Vector.V3
import Control.Applicative


-- * Initial values
origin :: Position
origin = Position (Vector3 0 0 0)

rand :: PureMT
rand = pureMT $ fromIntegral (0::Integer)


tests = [ ]

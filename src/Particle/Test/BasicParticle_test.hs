-- | A testing module for BasicParticle
module Particle.Test.BasicParticle_test (tests) where

-- Testing Libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck


-- The library under test
import Particle.BasicParticle

-- Its dependencies
import Space3DCartesian
import RandomValues
import Approx
import NumUnit
import System.Random.Mersenne.Pure64
import Data.Vector.V3
import Control.Applicative


tests = []
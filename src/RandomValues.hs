{-# LANGUAGE BangPatterns #-}

module RandomValues where

import Numerics ()

import System.Random.Mersenne.Pure64
import Data.Vector.V3
import Vectors

-- import Data.Vector.V3
import Data.List (unfoldr)

-- | Compute a random direction vector from a two random doubles
-- | TODO: Restrict these values to UnitInterval
sampleNormalVector3 :: Double -> Double -> Vector3
sampleNormalVector3 a b = sphericalToNormalVector3 (a*pi) (b*2*pi)

-- | Sample an exponential distribution from a random double
-- | TODO: Enforce lambda > 0 and a in UnitInterval
sampleExponential :: Double -> Double -> Double
sampleExponential lambda a = -log (a)*lambda



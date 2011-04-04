{-# LANGUAGE BangPatterns #-}

module RandomValues where

import SpaceTime.Space3DCartesian
import Numerics

import System.Random.Mersenne.Pure64
import Data.Vector.V3
import Vectors

-- import Data.Vector.V3
import Data.List (unfoldr)

newtype Seed = Seed { toInt :: Integer }
makePureMT :: Seed -> PureMT
makePureMT = pureMT . fromIntegral . toInt

-- | Compute a random direction vector from a two random doubles
-- | TODO: Restrict these values to UnitInterval
sampleNormalVector3 :: Double -> Double -> Vector3
sampleNormalVector3 a b = sphericalToNormalVector3 (a*pi) (b*2*pi)

-- | Sample an exponential distribution from a random double
-- | TODO: Enforce lambda > 0 and a in UnitInterval
sampleExponential :: Double -> Double -> Double
sampleExponential lambda a = -log (a)*lambda


-- | Compute a random Direction from a PureMT
randomDirection :: PureMT -> (Direction, PureMT)
randomDirection g = let
  (a, g')  = randomDouble g
  (b, g'') = randomDouble g'
  in (direction $ sampleNormalVector3 a b, g'')

-- | Sample an exponential Distance from a PureMT
randomExponential :: Double -> PureMT -> (Distance, PureMT)
randomExponential lambda g = let
  (a, g') = randomDouble g
  in (Distance $ sampleExponential lambda a, g')


-- | Get N samples from the given random function and PureMT
sampleN :: (PureMT -> (a, PureMT)) -> PureMT -> Int -> [a]
sampleN generator rand n
  | n <= 0 = []
  | otherwise =
    let (value, rand') = generator rand
    in value : ( sampleN generator rand' (n-1) )

samples :: (PureMT -> (a, PureMT)) -> PureMT -> [a]
samples generator rand = unfoldr (Just . generator) rand

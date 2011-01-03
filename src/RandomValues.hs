{-# LANGUAGE BangPatterns #-}

module RandomValues where

import Space

import System.Random.Mersenne.Pure64
import Data.Vector.V3

randomDirection_compute :: Double -> Double -> Direction
randomDirection_compute a b = let 
  theta = a * pi
  phi   = b * 2*pi
  x = (sin theta) * (cos phi)
  y = (sin theta) * (sin phi)
  z = (cos theta)
  in (Direction $ Vector3 x y z)

randomDirection :: PureMT -> (Direction, PureMT)
randomDirection g = let
  (a, g')  = randomDouble g
  (b, g'') = randomDouble g'
  in (randomDirection_compute a b, g'')


randomExponential_compute :: Double -> Double -> Double
randomExponential_compute lambda a = -log (a)*lambda

randomExponential :: Double -> PureMT -> (Distance, PureMT)
randomExponential lambda g = let
  (a, g') = randomDouble g
  in (Distance $ randomExponential_compute lambda a, g')


sampleN :: (PureMT -> (a, PureMT)) -> PureMT -> Int -> [a]
sampleN generator rand n 
  | n <= 0 = []
  | otherwise = 
    let (value, rand') = generator rand
    in value : ( sampleN generator rand' (n-1) )


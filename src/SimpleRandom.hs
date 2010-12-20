{-# LANGUAGE BangPatterns #-}

module SimpleRandom where

import Space

import System.Random.Mersenne.Pure64
import System.Random.Mersenne.Pure64.Base
import Control.Monad
import Control.Applicative
import Data.Vector.V3
import Data.Vector.Class

data Particle = InSpace { spPos :: Position, spDir :: Direction, spDist :: Distance, spRand :: PureMT }

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
randomExponential_compute lambda a = -log (a)/lambda

randomExponential :: Double -> PureMT -> (Double, PureMT)
randomExponential lambda g = let
  (a, g') = randomDouble g
  in (randomExponential_compute lambda a, g')




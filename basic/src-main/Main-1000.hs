{-# LANGUAGE BangPatterns #-}

-- | Main with 1000 cells

module Main where

import Control.Parallel.Strategies
import Data.List as L
import Data.Vector as V
import System.Environment

import Cell
import Material
import MC
import Mesh
import Physical
import PRNG
import Source
import Sphere1D
import Tally
import TryNSave
import Sigma_HBFC (nuE)

main :: IO ()
main =
  do
    [n,fname] <- getArgs
    writeTally fname (runManyParticles (read n) bigMesh)

-- | Perform the simulation for several (at least one) particles
-- in a given mesh.
runManyParticles :: Mesh m => Int -> m -> Tally
runManyParticles !n msh =
  let
    particles = genParticles n msh testRNG
    tallies   = L.map (runParticle nuE msh) particles
    chunked   = chunk 500 tallies
    res       = L.map (L.foldl1' merge) chunked
                `using` parBuffer 10 rdeepseq
  in
    L.foldl1' merge res

-- | Splits a lists into chunks of the given size. TODO: Reuse
-- library functions, or move elsewhere.
chunk :: Int -> [a] -> [[a]]
chunk n = L.unfoldr go
  where
    go xs = case splitAt n xs of
              ([], []) -> Nothing
              r        -> Just r

bigMesh :: Sphere1D
bigMesh =
  Sphere1D $ V.fromList $
     [Cell (Position 0) (Position 1)       Refl   Transp
           (Material (Opacity 0.1) (Opacity 2)
                     (Velocity 0) (Temperature 1)
                     (Density 1.0) (NDensity 0.5) (NDensity 0))]
     L.++
     [Cell (Position i) (Position $ i+1.0) Transp Transp
           (Material (Opacity 1) (Opacity 0.5)
                     (Velocity 0) (Temperature 2)
                     (Density 1.0) (NDensity 0.5) (NDensity 0))
     | i <- [1..998]]
     L.++
     [Cell (Position 999) (Position 1000)  Transp Refl
           (Material (Opacity 1)   (Opacity 0.5)
                     (Velocity 0) (Temperature 2)
                     (Density 1.0) (NDensity 0.5) (NDensity 0))]

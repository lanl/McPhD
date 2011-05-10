{-# LANGUAGE BangPatterns #-}
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

main :: IO ()
main =
  do
    [n,sz] <- getArgs
    print (runManyParticles (read n) (read sz) infMesh)

-- | Perform the simulation for several (at least one) particles
-- in a given mesh.
runManyParticles :: Mesh m => Int -> Int -> m -> Tally
runManyParticles !n !chunkSz msh =
  let
    particles = genParticles n msh testRNG
    tallies   = L.map (runParticle msh) particles
    chunked   = chunk chunkSz tallies
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

infMesh :: Sphere1D
infMesh = Sphere1D $ V.fromList
            [ Cell (Position 0) (Position 1) Refl   Transp
                   (Material (Opacity 0.1) (Opacity 2)
                             (Velocity 0) (Temperature 1)
                             (Density 1.0) (Density 0.5) (Density 0))
            , Cell (Position 1) (Position 2) Transp Refl
                   (Material (Opacity 1)   (Opacity 0.5)
                             (Velocity 0) (Temperature 2)
                             (Density 1.0) (Density 0.5) (Density 0))
            ]

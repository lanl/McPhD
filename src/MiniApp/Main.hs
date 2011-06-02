-- | A module for building a simple particle-transport application
-- with elastic, non-relativistic physics.
module Main where

import Data.Sequence as Seq
import Data.Array

import Space.Spherical1D

import Mesh.Classes
import Mesh.Spherical
import Properties
import Numerics
import MonteCarlo

import MiniApp.Particle
import MiniApp.Events as Events
import MiniApp.Model as Model

particles :: [Particle SphericalMesh]
particles = []

sphMesh :: SphericalMesh
sphMesh = SphericalMesh (Seq.fromList (fmap Radius [1..100])) Vacuum

cellData :: Physics Spherical1D
cellData = Physics { sig_abs  = Opacity 1.0
                   , sig_scat = Opacity 2.0
                   }

model :: Model SphericalMesh
model = Model { mesh    = sphMesh
              , physics = listArray (cellRange sphMesh) (repeat cellData)
              , t_final = Time 10.0
              }

streamOperator :: Particle SphericalMesh 
                  -> [(Event SphericalMesh, Particle SphericalMesh)]
streamOperator = stream (step model) Events.isFinalEvent

events :: [[(Event SphericalMesh, Particle SphericalMesh)]]
events = map streamOperator particles

main :: IO ()
main = do
  print "I did it!"

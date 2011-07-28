module Main where
-- | A module for building a simple particle-transport application
-- with elastic, non-relativistic physics.

import Data.Sequence as Seq
import Data.Array

import Coordinate.Spherical1D

import Mesh.Classes
import Mesh.Spherical
import Properties
import Numerics
import qualified MonteCarlo as MC

import MiniApp.Particle
import MiniApp.Events as Events
import MiniApp.Physics as Physics

import MiniApp.Model


-- Create a mesh
sphMesh :: SphericalMesh
sphMesh = SphericalMesh (Seq.fromList (fmap Radius [1..100])) Vacuum


-- Physical data
cellData :: Physics.Data Spherical1D
cellData = Physics.Data { sig_abs  = Opacity 1.0
                        , sig_scat = Opacity 2.0
                        }


-- Assemble the mesh, physics and final time into a model.
model :: Model SphericalMesh
model = Model { mesh    = sphMesh
              , physics = listArray (cellRange sphMesh) (repeat cellData)
              , t_final = Time 10.0
              }


-- Define our Monte Carlo operators for streaming a particle and creating a tally.

streamOp :: (Particle SphericalMesh)
            -> [MC.Outcome (Event SphericalMesh) (Particle SphericalMesh)]
streamOp = MC.stream (MC.step model contractors) Events.isFinalEvent


-- Create some particles
particles :: [Particle SphericalMesh]
particles = []





-- Fold tally function over lists of event lists.



-- Fold Tallies into complete tally


main :: IO ()
main = do
  print "I did it!"

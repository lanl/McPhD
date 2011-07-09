module Main where
-- | A module for building a simple particle-transport application
-- with elastic, non-relativistic physics, in Spherical Coordinates.
--


import Data.Sequence as Seq
import Data.Array
import Data.Monoid
import Data.Maybe

import qualified Mesh.Classes as Mesh
import Mesh.Spherical

import qualified Space.Classes as Space

import Properties
import Numerics
import NormalizedValues
import RandomNumbers
import qualified MonteCarlo as MC

import SphericalApp.Particle
import SphericalApp.Events as Events
import SphericalApp.Tally as Tally
import SphericalApp.Model


-- Create a mesh with 100 unit-sized cells and a vacuum boundary condition.
sphMesh :: SphericalMesh
sphMesh = SphericalMesh (Seq.fromList (fmap Radius [1..100])) Mesh.Vacuum


-- Physical data
cellData :: Material
cellData  = Material { sig_abs  = Opacity 1.0
                     , sig_scat = Opacity 2.0
                     }


-- Assemble the mesh, physics and final time into a model.
model :: Model
model = Model { mesh    = sphMesh
              , physics = listArray (Mesh.cellRange sphMesh) (repeat cellData)
              , t_final = Time 10.0
              }


-- Define our Monte Carlo operators for streaming a particle and creating a tally.

streamParticle :: Particle -> [MC.Outcome Event Particle]
streamParticle = MC.stream (MC.step model contractors) Events.isFinalEvent

tallyEvents :: [MC.Outcome Event Particle] -> Tally
tallyEvents = MC.monoidTally outcomeToTally

combineTally :: Tally -> Tally -> Tally
combineTally = mappend

initialTally :: Tally
initialTally = mempty

-- Create some test particles. Each particle is centered in its cell,
-- has the same energy, weight, time remaining, speed and
-- direction. The direction is orthogonal to the radial vector.
particles :: [Particle]
particles = map testParticle [0..99]

testParticle :: Int -> Particle
testParticle index = fromJust $ createParticle
                     sphMesh
                     (Space.make (location index) direction)
                     (Time 10.0)
                     (Energy 1.0)
                     (EnergyWeight 1.0)
                     (Speed 1.0)
                     (seed index)
  where
    direction = normalVector2 $ AzimuthAngle (pi/2)
    location index = Radius $ (fromIntegral index) + 0.5
    seed = Seed . fromIntegral


result :: Tally
result = MC.simulate (tallyEvents . streamParticle) combineTally initialTally particles

main :: IO ()
main = do
  print result

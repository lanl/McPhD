module Main where
-- | A module for building a simple particle-transport application
-- with elastic, non-relativistic physics, in Spherical Coordinates.
--


import Data.Sequence as Seq
import Data.Array
import Data.Monoid

import qualified Mesh.Classes as Mesh
import Mesh.Spherical
import Properties
import Numerics
import qualified MonteCarlo as MC

import SphericalApp.Particle
import SphericalApp.Events as Events
import SphericalApp.Physics as Physics
import SphericalApp.Tally as Tally

import SphericalApp.Model


-- Create a mesh with 100 unit-sized cells and a vacuum boundary condition.
sphMesh :: SphericalMesh
sphMesh = SphericalMesh (Seq.fromList (fmap Radius [1..100])) Mesh.Vacuum


-- Physical data
cellData :: Physics.Data
cellData = Physics.Data { sig_abs  = Opacity 1.0
                        , sig_scat = Opacity 2.0
                        }


-- Assemble the mesh, physics and final time into a model.
model :: Model
model = Model { mesh    = sphMesh
              , physics = listArray (Mesh.cellRange sphMesh) (repeat cellData)
              , t_final = Time 10.0
              }


-- Define our Monte Carlo operators for streaming a particle and creating a tally.

streamParticle :: Particle -> [(Event, Particle)]
streamParticle = MC.stream (MC.step model contractors) Events.isFinalEvent

tallyEvents :: [(Event, Particle)] -> Tally
tallyEvents = MC.monoidTally eventToTally

combineTally :: Tally -> Tally -> Tally
combineTally = mappend

initialTally :: Tally
initialTally = mempty

-- Create some test particles
particles :: [Particle]
particles = map (\index -> fromJust $ createParticle 
                           sphMesh 
                           (make location direction) 
                           (Time 10.0) 
                           (Energy 1.0) 
                           (EnergyWeight 1.0) 
                           (Speed 1.0) 
                           seed) [0..99]
            where
              location  = (fromIntgegral index) + 0.5
              direction = normalVector2 (pi/2)
              seed      = fromIntegral index



result :: Tally
result = MC.simulate (tallyEvents . streamParticle) combineTally initialTally particles

main :: IO ()
main = do
  print result

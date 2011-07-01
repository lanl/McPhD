{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, UndecidableInstances, NoMonomorphismRestriction, StandaloneDeriving #-}

module SphericalApp.Particle where

import Data.Function
import Control.Applicative
import System.Random.Mersenne.Pure64

import Mesh.Classes
import Mesh.Spherical

import Space.Classes
import Space.Spherical1D
import qualified Particle.Classes as P

import RandomSamples
import RandomNumbers
import Properties
import Utils.Combinators
import Approx

type CellT     = MeshCell SphericalMesh
type SpaceT    = Spherical1D
type VelocityT = Velocity Spherical1D

-- | Data type for a particle moving through a space with a
-- mesh. Indexed on the mesh itself.
data Particle = Particle
    {
      cell         :: !Cell             -- ^ Current cell in mesh.
    , location     :: !Space            -- ^ Location in mesh's space.
    , time         :: !Time             -- ^ Elapsed Time
    , energy       :: !Energy           -- ^ Particle energy
    , weight       :: !EnergyWeight     -- ^ Particle's energy weight
    , speed        :: !Speed            -- ^ Speed of motion.
    , rand         :: !PureMT           -- ^ Source of Particle's random behavior
    } deriving (Show)


instance P.Particle Particle where
-- | Move the particle the given distance. Assume cell and other
-- properties remain unchanged. Updating these has to be taken care of
-- by the model.
  move particle distance =
    let elapsedTime = goingAt distance (speed particle)
    in particle{ location = (location particle) +-> distance
               , time     = (time particle) + elapsedTime
               }

weightedEnergy :: Particle -> Energy
weightedEnergy particle = applyWeight (weight particle) (energy particle)

weightedMomentum :: Particle -> VelocityT
weightedMomentum particle =
  scale (location particle) (direction $ location particle) $
  (engwValue $ weight particle) * (spValue $ speed particle)

sampleDistance :: Opacity -> Particle -> (Distance, Particle)
sampleDistance opacity particle = let
  (distance, rng) = sampleExponential (1.0/(opValue opacity)) (rand particle)
  particle' = particle{rand=rng}
  in (Distance distance, particle')

instance Approx Particle where
    within_eps epsilon a b = close time
                             && close energy
                             && close location
                             && close weight
                             && close speed
                             && exact cell
      where close f = ((within_eps epsilon) `on` f) a b
            exact f = f a == f b

createParticle :: Spherical
                  -> Spherical1D
                  -> Time
                  -> Energy
                  -> EnergyWeight
                  -> Speed
                  -> Seed
                  -> Maybe Particle
createParticle mesh location time energy weight speed seed =
  Particle <$> cell
  <*^> location
  <*^> time
  <*^> energy
  <*^> weight
  <*^> speed
  <*^> (makePureMT seed)
  where cell = cell_find mesh location

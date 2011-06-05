{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, UndecidableInstances, NoMonomorphismRestriction, StandaloneDeriving #-}

module MiniApp.Particle where

import Data.Function
import Control.Applicative
import System.Random.Mersenne.Pure64

import Mesh.Classes
import Space.Classes
import qualified Particle.Classes as P

import RandomSamples
import RandomNumbers
import Properties
import Utils.Combinators
import Approx

-- | Data type for a particle moving through a space with a
-- mesh. Indexed on the mesh itself.
data (Mesh mesh) => Particle mesh = Particle
    {
      cell         :: !(MeshCell mesh)  -- ^ Current cell in mesh.
    , location     :: !(MeshSpace mesh) -- ^ Location in mesh's space.
    , time         :: !Time             -- ^ Elapsed Time
    , energy       :: !Energy           -- ^ Particle energy
    , weight       :: !EnergyWeight     -- ^ Particle's significance weighting
    , speed        :: !Speed            -- ^ Speed of motion.
    , rand         :: !PureMT           -- ^ Source of Particle's random behavior
    }

instance (Mesh m) => P.Particle (Particle m) where
-- | Move the particle the given distance. Assume cell and other
-- properties remain unchanged. Updating these has to be taken care of
-- by the model.
  move particle distance =
    let elapsedTime = goingAt distance (speed particle)
    in particle{ location = (location particle) +-> distance
               , time     = (time particle) + elapsedTime
               }

weightedEnergy :: (Mesh m) => Particle m -> Energy
weightedEnergy particle = applyWeight (weight particle) (energy particle)

weightedMomentum :: (Mesh m) => Particle m -> Momentum (MeshSpace m)
weightedMomentum particle = Momentum
                            (engValue $ weightedEnergy particle)
                            (direction $ location particle)

deriving instance ( Mesh mesh
                  , Show (MeshSpace mesh)
                  , Show (MeshCell mesh)) => Show (Particle mesh)

sampleDistance :: (Mesh m) => Opacity -> Particle m -> (Distance, Particle m)
sampleDistance opacity particle = let
  (distance, rng) = sampleExponential (1.0/(opValue opacity)) (rand particle)
  particle' = particle{rand=rng}
  in (Distance distance, particle')

instance (Approx (MeshSpace mesh), Mesh mesh) => Approx (Particle mesh) where
    within_eps epsilon a b = close time
                             && close energy
                             && close location
                             && close weight
                             && close speed
                             && exact cell
      where close f = ((within_eps epsilon) `on` f) a b
            exact f = f a == f b

createParticle :: (Mesh m) => m
                      -> (MeshSpace m)
                      -> Time
                      -> Energy
                      -> EnergyWeight
                      -> Speed
                      -> Seed
                      -> Maybe (Particle m)
createParticle mesh location time energy weight speed seed =
  Particle <$> cell
  <*^> location
  <*^> time
  <*^> energy
  <*^> weight
  <*^> speed
  <*^> (makePureMT seed)
  where cell = cell_find mesh location

{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, UndecidableInstances, NoMonomorphismRestriction, StandaloneDeriving #-}

module Particle.MeshedParticle where

import Data.Function
import Control.Applicative
import System.Random.Mersenne.Pure64

import Mesh.Classes
import Space.Classes
import RandomNumbers
import Properties
import Approx

-- | A variation on 'apply' which lifts the second argument.
(<*^>) :: (Applicative f) => f (a -> b) -> a -> f b
(<*^>) g a = g <*> (pure a)
infixl 4 <*^>


-- | Data type for a particle moving through a space with a
-- mesh. Indexed on the mesh itself.
data (Mesh mesh) => MeshParticle mesh = MeshParticle
    {
      pimCell         :: !(MeshCell mesh)  -- ^ Current cell in mesh.
    , pimLocation     :: !(MeshSpace mesh) -- ^ Location in mesh's space.
    , pimTime         :: !Time             -- ^ Elapsed Time
    , pimEnergy       :: !Energy           -- ^ Particle energy
    , pimEnergyWeight :: !EnergyWeight     -- ^ Particle's weighted energy
    , pimSpeed        :: !Speed            -- ^ Speed of motion.
    , pimRand         :: !PureMT           -- ^ Source of Particle's random behavior
    }

-- | Move the particle the given distance. Assume cell and other
-- properties remain unchanged. Updating these has to be taken care of
-- by the model.
move :: (Mesh m) => MeshParticle m -> Distance -> MeshParticle m
move particle distance =
    let elapsedTime = goingAt distance (pimSpeed particle)
        time        = pimTime particle
        location    = pimLocation particle
    in particle{ pimLocation = location +-> distance
               , pimTime     = time + elapsedTime
               }

pimWeightedEnergy :: (Mesh m) => MeshParticle m -> Energy
pimWeightedEnergy particle = applyWeight (pimEnergyWeight particle) (pimEnergy particle)

pimWeightedMomentum :: (Mesh m) => MeshParticle m -> Momentum (MeshSpace m)
pimWeightedMomentum particle = Momentum (engValue $ pimWeightedEnergy particle) (direction $ pimLocation particle)

deriving instance ( Mesh mesh
                  , Show (MeshSpace mesh)
                  , Show (MeshCell mesh)) => Show (MeshParticle mesh)


createMeshParticle :: (Mesh m) => m
                      -> (MeshSpace m)
                      -> Time
                      -> Energy
                      -> EnergyWeight
                      -> Speed
                      -> Seed
                      -> Maybe (MeshParticle m)
createMeshParticle mesh location time energy energyWeight speed seed =
  MeshParticle <$> cell
  <*^> location
  <*^> time
  <*^> energy
  <*^> energyWeight
  <*^> speed
  <*^> (makePureMT seed)
  where cell = cell_find mesh location

instance (Approx (MeshSpace mesh), Mesh mesh) => Approx (MeshParticle mesh) where
    within_eps epsilon a b = close pimTime
                             && close pimEnergy
                             && close pimEnergyWeight
                             && close pimSpeed
                             && exact pimCell
      where close f = ((within_eps epsilon) `on` f) a b
            exact f = f a == f b
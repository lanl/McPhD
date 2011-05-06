{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-| A particle type which is paramaterized over the space it moves in.
-}
module Particle.ParametricParticle where

import Data.Functor
import Control.Applicative
import System.Random.Mersenne.Pure64

import SpaceTime.Classes
import Mesh.Classes
import RandomNumbers
import Numerics
import Approx

import SpaceTime.Test.Space_arbitrary ()

-- | Data type for a particle moving through a space with a mesh.
data (SpaceMesh mesh) => ParticleInMesh mesh = ParticleInMesh
    {
      ppmCell     :: MeshCell mesh  -- ^ Current cell in mesh.
    , ppmLocation :: MeshSpace mesh -- ^ Location in mesh's space.
    , ppmTime     :: Time
    , ppmRand     :: PureMT
    }

-- | A variant on 'apply' which lifts the second argument.
(<*^>) :: (Applicative f) => f (a -> b) -> a -> f b
(<*^>) g a = g <*> (pure a)
infixl 4 <*^>

createParticleInMesh :: (SpaceMesh m) => m -> (MeshSpace m)
                        -> Time
                        -> Seed
                        -> Maybe (ParticleInMesh m)
createParticleInMesh mesh location time seed =
  ParticleInMesh <$> cell <*^> location <*^> time <*^> (makePureMT seed)
  where cell = cell_find mesh location

instance (Approx (MeshSpace mesh), SpaceMesh mesh) =>
         Approx (ParticleInMesh mesh) where
  within_eps epsilon a b =
    (weps (ppmLocation a) (ppmLocation b)) && (weps (ppmTime a) (ppmTime b))
    where weps :: Approx a => a -> a -> Bool
          weps = within_eps epsilon



-- | Data type for a particle moving through space.
data ParticleInSpace space = ParticleInSpace
    {
      ppLocation :: space -- ^ Location in Space
    , ppTime :: Time      -- ^ Time in flight.
    , ppRand :: PureMT    -- ^ RNG.
    } deriving Show

createParticleInSpace :: (Space a) => a -> Time -> Seed -> ParticleInSpace a
createParticleInSpace location time seed =
  ParticleInSpace location time (makePureMT seed)

instance (Approx space) => Approx (ParticleInSpace space) where
  within_eps epsilon a b =
    (weps (ppLocation a) (ppLocation b)) && (weps (ppTime a) (ppTime b))
    where weps :: Approx a => a -> a -> Bool
          weps = within_eps epsilon

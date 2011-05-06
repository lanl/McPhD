{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, UndecidableInstances, NoMonomorphismRestriction #-}
{-| A particle type which is paramaterized over the space it moves in.
-}
module Particle.ParametricParticle where

import Data.Functor
import Data.Function
import Data.Ix
import Control.Applicative
import System.Random.Mersenne.Pure64

import Space.Classes
import Mesh.Classes
import RandomNumbers
import Numerics
import Approx

import Space.Test.Space_arbitrary ()

-- | A variant on 'apply' which lifts the second argument.
(<*^>) :: (Applicative f) => f (a -> b) -> a -> f b
(<*^>) g a = g <*> (pure a)
infixl 4 <*^>


-- | Data type for particles with an index and a space.  This rediuces
-- the coupling to details of the mesh. Including any other type
-- parameters that may be added to it.
data (Ix i, Space s) => ParticleSpaceIndex i s = ParticleSpaceIndex
    { 
      piIndex    :: i
    , piLocation :: s
    , piTime     :: Time
    , piRand     :: PureMT
    } deriving Show

instance (Ix i, Space s, Approx s) => Approx (ParticleSpaceIndex i s) where
    within_eps epsilon a b = (weps `on` piLocation) a b && 
                             (weps `on` piTime) a b && 
                             (piIndex a == piIndex b)
        where weps = within_eps epsilon





-- | Data type for a particle moving through a space with a mesh.
data (Mesh mesh) => ParticleInMesh mesh = ParticleInMesh
    { 
      ppmCell     :: MeshCell mesh  -- ^ Current cell in mesh.
    , ppmLocation :: MeshSpace mesh -- ^ Location in mesh's space.
    , ppmTime     :: Time
    , ppmRand     :: PureMT
    }  -- deriving Show  ??? Can't get the correct instance declaration for this to work.

createParticleInMesh :: (Mesh m) => m -> (MeshSpace m)
                        -> Time
                        -> Seed
                        -> Maybe (ParticleInMesh m)
createParticleInMesh mesh location time seed =
  ParticleInMesh <$> cell <*^> location <*^> time <*^> (makePureMT seed)
  where cell = cell_find mesh location

instance (Approx (MeshSpace mesh), Mesh mesh) => Approx (ParticleInMesh mesh) where
    within_eps epsilon a b = (weps `on` ppmLocation) a b && 
                             (weps `on` ppmTime) a b && 
                             (ppmCell a == ppmCell b)
        where weps = within_eps epsilon





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
  within_eps epsilon a b = (weps `on` ppLocation) a b && 
                           (weps `on` ppTime) a b
    where weps = within_eps epsilon

{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, UndecidableInstances, NoMonomorphismRestriction, StandaloneDeriving #-}
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
import Properties
import Approx

import Space.Test.Space_arbitrary ()

-- | A variation on 'apply' which lifts the second argument.
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
    , piSpeed    :: Speed
    , piRand     :: PureMT
    } deriving Show

instance (Ix i, Space s, Approx s) => Approx (ParticleSpaceIndex i s) where
    within_eps epsilon a b = (weps `on` piLocation) a b && 
                             (weps `on` piTime) a b && 
                             (piIndex a == piIndex b)
        where weps = within_eps epsilon



-- | Data type for a particle moving through a space with a
-- mesh. Indexed on the mesh itself.
data (Mesh mesh) => ParticleInMesh mesh = ParticleInMesh
    { 
      pimCell     :: MeshCell mesh  -- ^ Current cell in mesh.
    , pimLocation :: MeshSpace mesh -- ^ Location in mesh's space.
    , pimTime     :: Time           -- ^ Elapsed Time
    , pimSpeed    :: Speed          -- ^ Speed of motion (location contains direction)
    , pimRand     :: PureMT         -- ^ Source of Particle's random behavior
    }
    
-- | Move the particle the given distance. Assume cell remains unchanged.
move :: (Mesh m) => ParticleInMesh m -> Distance -> ParticleInMesh m
move particle distance = 
    let elapsedTime = undefined
        time        = pimTime particle
        location    = pimLocation particle
    in particle{ pimLocation = location +-> distance
               , pimTime     = time + elapsedTime
               }
                                        
                                        
deriving instance ( Mesh mesh
                  , Show (MeshSpace mesh)
                  , Show (MeshCell mesh)) => Show (ParticleInMesh mesh)


createParticleInMesh :: (Mesh m) => m -> (MeshSpace m)
                        -> Time
                        -> Speed
                        -> Seed
                        -> Maybe (ParticleInMesh m)
createParticleInMesh mesh location time speed seed =
  ParticleInMesh <$> cell <*^> location <*^> time <*^> speed <*^> (makePureMT seed)
  where cell = cell_find mesh location

instance (Approx (MeshSpace mesh), Mesh mesh) => Approx (ParticleInMesh mesh) where
    within_eps epsilon a b = (weps `on` pimLocation) a b && 
                             (weps `on` pimTime) a b && 
                             (pimCell a == pimCell b)
        where weps = within_eps epsilon



-- | Data type for a particle moving through space. No mesh or mesh index.
data ParticleInSpace space = ParticleInSpace
    {
      ppLocation :: space  -- ^ Location in Space
    , ppTime     :: Time   -- ^ Time in flight.
    , ppSpeed    :: Speed  -- ^ Speed of motion
    , ppRand     :: PureMT -- ^ RNG.
    } deriving Show

createParticleInSpace :: (Space a) => a -> Time -> Speed -> Seed -> ParticleInSpace a
createParticleInSpace location time speed seed =
  ParticleInSpace location time speed (makePureMT seed)

instance (Approx space) => Approx (ParticleInSpace space) where
  within_eps epsilon a b = (weps `on` ppLocation) a b && 
                           (weps `on` ppTime) a b
    where weps = within_eps epsilon

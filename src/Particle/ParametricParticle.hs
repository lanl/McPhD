{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-| A particle type which is paramaterized over the space it moves in.
-}
module Particle.ParametricParticle where

import System.Random.Mersenne.Pure64

import SpaceTime.Classes
import Mesh.Classes
import RandomNumbers
import Numerics
import Approx

import SpaceTime.Test.Space_arbitrary ()


{-- ???: How useful is this? If I add mesh information by wrapping
this class, I run a risk of inconsistent spaces:

  data MeshedP m s = MeshedP { particle :: (ParametricParticle s), mesh :: m }

what happens if MeshSpace m != s? E.g.

  MeshedP Cartesian3D Spherical1D

Surely, it would fail to compile, but where, and would the error
message be helpful?

If I genuinely need particles in free space without a mesh, I can
create a "null" mesh with a single cell and trivial operations. The
mesh would still be useful for defining a boundary on the space.

--}

-- !!!: In this case, if you really want MeshSpace m = s, then why not
--
-- data MeshedP m = MeshedP { particle :: (ParametricParticle (Meshspace m)),
--                            mesh     :: m }
--

data ParametricParticle space =
  ParametricParticle
  {
    ppLocation :: space -- ^ Where is it?
  , ppTime :: Time      -- ^ Time in flight.
  , ppRand :: PureMT    -- ^ RNG.
  } deriving Show

createParametricParticle :: (Space a) => a -> Time -> Seed -> ParametricParticle a
createParametricParticle location time seed =
  ParametricParticle location time (makePureMT seed)

-- ??? Cannot re-use the function weps for comparing Time values. The
-- error is a mismatch between space and Time. The first use of weps
-- appears to fix its type signature.

-- !!!: That's a tricky corner of Haskell. You've been hit by the
-- "monomorphism restriction", which is controversial at best. Bindings
-- without a type signature and without function arguments (such as weps)
-- must not be overloaded. The restriction is there because the definition
-- looks like a constant (evaluated only once), but in practice behaves
-- like a function (evaluated on every call). Options around it: add
-- a type signature (what I did), add a function argument (would work in
-- this case, too, but not for truly overloaded non-functions), or remove
-- the monomorphism restriction for the module by using the
-- NoMonomorphismRestriction LANGUAGE pragma.

instance (Approx space) => Approx (ParametricParticle space) where
  within_eps epsilon a b =
    (weps (ppLocation a) (ppLocation b)) && (weps (ppTime a) (ppTime b))
    where weps :: Approx a => a -> a -> Bool
          weps = within_eps epsilon


data (SpaceMesh mesh) => ParamParticleMesh mesh = ParamParticleMesh
    {
      ppmLocation :: MeshSpace mesh  -- ^ Location is mesh's space.
    , ppmTime :: Time
    , ppmRand :: PureMT
    }

createParamParticleMesh :: (SpaceMesh m) => (MeshSpace m) -> Time -> Seed -> ParamParticleMesh m
createParamParticleMesh location time seed = ParamParticleMesh location time (makePureMT seed)

-- ??? Can't re-use weps here either, even if I specify it's parametric definition.
-- !!!: Yes, you need the type signature as a real type signature, not just
-- as an annotation.
instance (Approx (MeshSpace mesh), SpaceMesh mesh) => Approx (ParamParticleMesh mesh) where
  within_eps epsilon a b =
    (weps (ppmLocation a) (ppmLocation b)) && (within_eps epsilon (ppmTime a) (ppmTime b))
    where weps :: Approx a => a -> a -> Bool
          weps = within_eps epsilon

{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

module MiniApp.Model where
{-| MiniApp Model

Provide a function which returns an event arising from the interaction
of the particle with the material at each step.

-}

import Data.Array.IArray

import Particle.MeshedParticle
import Mesh.Classes
import qualified Space.Classes as Space
import Space.Classes as Space hiding (Motion)
import Properties

import MiniApp.Events

-- * Aliases
type P = MeshParticle

-- | Outcomes are Event, Particle pairs that might happen (candidates)
-- or finally do happen.
type Outcome m = (Event m, P m)

-- | Properties of the space.
data (Space s) => Physics s = Physics {
      sig_abs   :: !Opacity
    , sig_scat  :: !Opacity
    , mvel      :: !(Velocity s)
    , tempE     :: !Temperature
    , rhoNucl   :: !Density
    , rhoEMinus :: !NDensity
    , rhoEPlis  :: !NDensity
    }

deriving instance (Space s, Eq   (Velocity s)) => Eq   (Physics s)
deriving instance (Space s, Show (Velocity s)) => Show (Physics s)

-- | The physical model. Consists of a mesh, space properties indexed
-- by mesh cell and the end of time-step
data (Mesh m) => Model m = Model {
      mesh    :: m
    , physics :: Array (MeshCell m) (Physics (MeshSpace m))
    , t_final :: Time
    }

-- | Extract the physics data for the Particle's cell.
localPhysics :: (Mesh m) => Model m -> P m -> Physics (MeshSpace m)
localPhysics model particle = (physics model) ! (pimCell particle)


-- | Compute an Outcome for reaching the timestep end.
-- TODO: This is a lot of bookkeeping for the simplest limiter!
timeStepEnd_dist :: (Mesh m) => Model m -> P m -> Outcome m
timeStepEnd_dist model particle =
    let time_left     = t_final model - pimTime particle
        location      = pimLocation particle
        distance      = gettingTo time_left (pimSpeed particle)
        motion        = Motion $ Space.Motion location distance
        particle'     = move particle distance
        limiter       = Timeout
    in (Events [motion, limiter], particle')

-- | Create an event for absorption.
absorption_event :: (Mesh m) => Model m -> P m -> Distance -> Outcome m
absorption_event particle distance = undefined
  -- let particle' = move particle distance
  --     motion    = Motion (pimLocation particle) distance



-- | Compute an Outcome for scattering
scatter_dist :: (Mesh m) => Model m -> P m -> Outcome m
scatter_dist = undefined
  -- let sigma_abs  = sig_abs  $ local_physics model
  --     sigma_scat = sig_scat $ local_physics model


-- | Compute and Outcome for mesh crossing events.
boundary_dist :: (Mesh m) => Model m -> P m -> Outcome m
boundary_dist = undefined

step :: (Mesh m) => Model m -> MeshParticle m -> Outcome m
step = undefined

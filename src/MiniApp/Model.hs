{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

module MiniApp.Model where
{-| MiniApp Model

Provide a function which returns an event arising from the interaction
of the particle with the material at each step.

-}

import Data.Array.IArray

import RandomSamples
import MiniApp.Particle
import Mesh.Classes

import qualified Space.Classes as Space
import Space.Classes as Space hiding (Motion)

import qualified Particle.Classes as P
import Properties

import MiniApp.Events

-- | Outcomes are a Distance, Event and Particle which might happen
-- (candidates) or finally do happen. 
data Outcome m = Outcome { distance :: !Distance
                         , event    :: (Event m)
                         , particle :: Particle m }

-- | Properties of the material.
data (Space s) => Physics s = Physics {
      sig_abs   :: !Opacity
    , sig_scat  :: !Opacity
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
localPhysics :: (Mesh m) => Model m -> Particle m -> Physics (MeshSpace m)
localPhysics model particle = (physics model) ! (cell particle)


-- | Compute an Outcome for reaching the timestep end.
-- TODO: This is a lot of bookkeeping for the simplest limiter!
timeStepEnd_outcome :: (Mesh m) => Model m -> Particle m -> Outcome m
timeStepEnd_outcome model particle =
    let time_left     = t_final model - time particle
        distance      = gettingTo time_left (speed particle)
        motion        = Motion $ Space.Motion (location particle) distance
        particle'     = P.move particle distance
        limiter       = Timeout
    in Outcome distance (Events [motion, limiter]) particle'


-- | Create an event for absorption.
absorption_outcome :: (Mesh m) => Model m -> Particle m -> Outcome m
absorption_outcome model particle = 
  let opacity         = sig_abs $ localPhysics model particle 
      (distance, rng) = sampleExponential (1.0/(opValue opacity)) (rand particle)
      motion          = Motion $ Space.Motion (location particle) (Distance distance)
      particle'       = P.move particle (Distance distance)
      particle''      = particle'{rand = rng}
      momentum        = weightedMomentum particle' -- ^ All momentum despoited
      energy          = weightedEnergy particle' -- ^ All energy deposited
      limiter         = Collide Scatter momentum energy
  in Outcome (Distance distance) (Events [motion, limiter]) particle''


-- | Compute an Outcome for scattering
scatter_dist :: (Mesh m) => Model m -> Particle m -> Outcome m
scatter_dist = undefined
  -- let sigma_abs  = sig_abs  $ local_physics model
  --     sigma_scat = sig_scat $ local_physics model


-- | Compute and Outcome for mesh crossing events.
boundary_dist :: (Mesh m) => Model m -> Particle m -> Outcome m
boundary_dist = undefined

step :: (Mesh m) => Model m -> Particle m -> (Event m, Particle m)
step = undefined

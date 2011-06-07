{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

module MiniApp.Model where
{-| MiniApp Model

Defines the Model, which is the combination of data and operations
which are sufficient to determine the next event in a particle history.

-}

import Data.Array.IArray

import Mesh.Classes
import qualified Particle.Classes as P

import Properties

import MiniApp.Particle
import MiniApp.Events
import MiniApp.Physics
import MiniApp.Outcome


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


-- | Advance the particle one step, according to the model.
--
-- This function could be quite general, if given a list of functions
-- which produce outcomes. (Contractors)
step :: (Mesh m) => Model m -> Particle m -> (Event m, Particle m)
step model particle =
    -- | Compute the outcome of physics, the mesh and the timestep.
    let outcomes = [ physicsOutcome (localPhysics model particle) particle
                   , timeStepOutcome model particle
                   , meshOutcome model particle]
    -- | Smallest distance wins.
    in result (foldl1 min outcomes)


-- * Functions which compute particular kinds of outcomes, resulting
-- from interaction with the mesh, the timestep and the medium. Each
-- of these has the same type:
--
--   Model m -> Particle m -> Outcome m

-- | Compute an Outcome for reaching the timestep end.
timeStepOutcome :: (Mesh m) => Model m -> Particle m -> Outcome m
timeStepOutcome model particle =
    let time_left = t_final model - time particle
        distance  = gettingTo time_left (speed particle)
        particle' = P.move particle distance
    in Outcome distance Timeout particle'

-- | Dispatch to the mesh's distance to boundary functions.
meshOutcome :: (Mesh m) => Model m -> Particle m -> Outcome m
meshOutcome model particle = undefined


-- materialOutcome :: (Mesh m) => Contractor

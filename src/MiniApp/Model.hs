{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

module MiniApp.Model where
{-| MiniApp Model

Defines the Model, which is the combination of data and operations
which are sufficient to determine the next event in a particle history.

Note that while this model is written to be polymorphic over the mesh,
this is not required in general.

-}

import Data.Array.IArray
import Data.Monoid


import NormalizedValues
import qualified Space.Classes as S

import Mesh.Classes
import qualified Particle.Classes as P

import Properties
import qualified MonteCarlo as MC

import MiniApp.Particle
import MiniApp.Events
import MiniApp.Physics


-- Aliases

weightedMomentum :: (Mesh m) => Particle m -> Momentum (MeshSpace m)
weightedMomentum particle = Momentum $ Quot 
                            (engValue $ weightedEnergy particle)
                            (S.direction $ location particle)


-- Aliases for the MonteCarlo types we need.
type Outcome m    = MC.Outcome    (Event m) (Particle m)
type Contractor m = MC.Contractor (Model m) (Particle m) (Event m)


-- | The physical model. Consists of a mesh, space properties indexed
-- by mesh cell and the end of time-step
data (Mesh m) => Model m = Model {
      mesh    :: m
    , physics :: Array (MeshCell m) (Data (MeshSpace m))
    , t_final :: Time
    }

-- | Extract the physics data for the Particle's cell.
localPhysics :: (Mesh m) => Model m -> Particle m -> Data (MeshSpace m)
localPhysics model particle = (physics model) ! (cell particle)

-- * Functions which compute outcomes, resulting from interaction with
-- the mesh, the timestep and the medium. Each of these has the same
-- type, called Contractor:
--   Model m -> Particle m -> Outcome m

-- | Compute an Outcome for reaching the timestep end.
timeStepContractor :: (Mesh m) => Contractor m
timeStepContractor model particle =
    let time_left = t_final model - time particle
        distance  = gettingTo time_left (speed particle)
        particle' = P.move particle distance
    in MC.Outcome distance Timeout particle'

-- | Dispatch to the mesh's distance to boundary functions.
meshContractor :: (Mesh m) => Contractor m
meshContractor = undefined


materialContractor :: (Mesh m) => Contractor m
materialContractor = undefined


-- | A list of contractors that we hand to the step function.
contractors :: (Mesh m) => [Contractor m]
contractors = [timeStepContractor, meshContractor, materialContractor]



-- | Tallies

-- | Information tallied in each cell.
data (Mesh m) => CellTally m = CellTally !(Momentum m) !Energy
deriving instance (Mesh m, Show (Momentum m)) => Show (CellTally m)

-- Need addition operators for momentum and energy for this.
-- instance (Mesh m) => Monoid (CellTally m) where
--     mempty = CellTally 0 0
--     mappend (CellTally m1 e1) (CellTally m2 e2) = CellTally (m1+m2) (e1+e2)

data EventCount = EventCount {
      nEscape  :: !Int
    , nReflect :: !Int
    , nTimeout :: !Int
    }

instance Monoid EventCount where
  mempty = EventCount 0 0 0
  mappend (EventCount ne1 nr1 nt1) (EventCount ne2 nr2 nt2) =
      EventCount (ne1+ne2) (nr1+nr2) (nt1+nt2)


{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

module SphericalApp.Model where
{-| SphericalApp Model

Defines the Model, which is the combination of data and operations
which are sufficient to determine the next event in a particle history.

Note that while this model is written to be polymorphic over the mesh,
this is not required in general.

-}

import Data.Array.IArray

import Mesh.Classes as Mesh
import Mesh.Spherical

import qualified Particle.Classes as Particle

import Properties
import qualified MonteCarlo as MC

import SphericalApp.Particle
import SphericalApp.Events
import SphericalApp.Physics


-- * Aliases for the MonteCarlo types.

type Cell        = Mesh.MeshCell SphericalMesh
type Outcome     = MC.Outcome    Event Particle
type Contractor  = MC.Contractor Model Particle Event


-- | The physical model. Consists of a mesh, space properties indexed
-- by mesh cell and the end of time-step
data Model = Model {
      mesh    :: SphericalMesh
    , physics :: Array Cell Data
    , t_final :: Time
    }

-- | Extract the physics data for the Particle's cell.
localPhysics :: Model -> Particle -> Data
localPhysics model particle = (physics model) ! (cell particle)

-- * Functions which compute outcomes, resulting from interaction with
-- the mesh, the timestep and the medium. Each of these has the same
-- type, called Contractor:
--   Model m -> Particle m -> Outcome m

-- | Compute an Outcome for reaching the timestep end.
timeStepContractor :: Contractor
timeStepContractor model particle =
    let time_left = t_final model - time particle
        distance  = gettingTo time_left (speed particle)
        particle' = Particle.move particle distance
    in MC.Outcome distance Timeout particle'


-- | Contractor for face and boundary crossings in the mesh.
meshContractor :: Contractor
meshContractor = undefined


-- | Contractor for scattering event
scatteringContractor :: Contractor
scatteringContractor model particle =
  let Data{sig_scat=opacity} = localPhysics model particle
      (distance, particle')  = sampleDistance opacity particle
      particle''             = isotropicScatter particle'
      event = Collide
              { collideType = Scatter
              , momentumDep = (weightedMomentum particle - weightedMomentum particle'')
              , energyDep   = (weightedEnergy   particle - weightedEnergy particle'')
              }
  in MC.Outcome distance event particle''

isotropicScatter = id

-- | Contractor for scattering event
absorptionContractor :: Contractor
absorptionContractor model particle =
  let Data{sig_abs=opacity} = localPhysics model particle
      (distance, particle') = sampleDistance opacity particle
      event = Collide
              { collideType = Absorb
              , momentumDep = (weightedMomentum particle')
              , energyDep   = (weightedEnergy   particle')
              }
  in MC.Outcome distance event particle'



-- | A list of contractors that we hand to the step function.
contractors :: [Contractor]
contractors = [ timeStepContractor
              , meshContractor
              , scatteringContractor
              , absorptionContractor
              ]

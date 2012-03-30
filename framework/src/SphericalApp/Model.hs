{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

module SphericalApp.Model where
{-| SphericalApp Model

Defines the Model, which is the combination of _data_ and _contractors_
which are sufficient to determine the next event in a particle history.

The Model's data is a mesh, a time-step value and an array, indexed by
the mesh's cell indices, of Material data. The Material data, in tern,
contains an absorption and scattering opacity.

The contractors are functions which compute the particle's streaming
distance to various events, such as mesh boundaries, end of time-step,
and material interactions, _and_ the result of this interaction.

These contractors are then provided to the MC module, which will
evaluate them, compare the results, and continue the iteration.

-}

import Data.Array.IArray

import Mesh.Classes as Mesh
import Mesh.Spherical

import qualified Particle.Classes as Particle

import Properties
import Numerics
import qualified MonteCarlo as MC

import SphericalApp.Particle
import SphericalApp.Events

-- * Aliases for paramaterized types, specialized on the Mesh,
-- Particle and Events for this App.
type Cell        = Mesh.MeshCell  SphericalMesh
type Face        = Mesh.MeshFace  SphericalMesh
type Neighbor    = Mesh.NeighborT SphericalMesh
type Outcome     = MC.Outcome     Event Particle
type Contractor  = MC.Contractor  Model Event Particle

-- | Properties of the material we're streaming through.
data Material = Material { 
      sig_abs  :: !Opacity -- ^ Absorption opacity
    , sig_scat :: !Opacity -- ^ Absorption opacity
    }


-- | The physical model. Consists of a mesh, space properties indexed
-- by mesh cell and the end of time-step
data Model = Model {
      mesh    :: SphericalMesh
    , physics :: Array Cell Material  -- ^ Each cell has different material properties.
    , t_final :: Time
    }

-- | Extract the physics data for the Particle's cell.
localPhysics :: Model -> Particle -> Material
localPhysics model particle = (physics model) ! (cell particle)

-- * Contractors 
--
-- Functions which compute outcomes that result from interaction with
-- the mesh, the timestep and the medium. 

-- | Compute an Outcome for reaching the timestep end.
timeStepContractor :: Contractor
timeStepContractor model particle =
    let time_left = t_final model - time particle
        distance  = gettingTo time_left (speed particle)
        particle' = Particle.move particle distance
    in MC.Outcome distance Timeout particle'


-- | Contractor for face and boundary crossings in the mesh.
meshContractor :: Contractor
meshContractor model particle =
    let m = mesh model
        c = cell particle
        l = location particle
        crossing = cell_boundary m c l
    in makeOutcome crossing
        where makeOutcome (distance, neighbor) =
                  let event     = convertNeighbor neighbor
                      particle' = Particle.move particle distance
                  in MC.Outcome distance event particle'


-- | Contractor for scattering event
scatteringContractor :: Contractor
scatteringContractor model particle =
  let Material{sig_scat=opacity} = localPhysics model particle
      (distance, particle')      = sampleDistance opacity particle
      particle''                 = Particle.move particle distance
      particle'''                = isotropicScatter particle''
      event = Collide
              { collideType = Scatter
              , momentumDep = (weightedMomentum particle - weightedMomentum particle''')
              , energyDep   = (weightedEnergy   particle - weightedEnergy   particle''')
              }
  in MC.Outcome distance event particle'''

-- TODO: This needs fixing.
isotropicScatter = id

-- | Contractor for absorption event
absorptionContractor :: Contractor
absorptionContractor model particle =
  let Material{sig_abs=opacity} = localPhysics model particle
      (distance, particle')     = sampleDistance opacity particle
      event = Collide
              { collideType = Absorb
              , momentumDep = (weightedMomentum particle')
              , energyDep   = (weightedEnergy   particle')
              }
  in MC.Outcome distance event particle'


-- * Converters from mesh crossings to Events

-- Do I need mesh crossing events which are distinct from the mesh
-- neighbor types?
--
-- Face means the same thing in both contexts.
--
-- There may be more than one kind of Boundary. E.g. physical versus
-- computational domain. The latter would probably just be tallied as
-- a face crossing. If a physical boundary is not Escape, what could it be?
--
-- Self might mean more than Reflect. E.g. hitting the origin in
-- Spherical coordaintes and still being in cell 0. Although, this
-- could be considered a kind of reflection.
--
-- It it appears that Events are different than the mesh's neighbor
-- relationships, but in this model, they look very much alike.

-- You can argue that the tally should record _exactly_ the events
-- which occur in the course of the simulation. Conversion to other
-- quantities should happen in post-processing. This would mean
-- refining the neighbor kinds to discriminate between all events

-- | Map mesh Neighbor constructors to BoundaryType constructors
convertMeshCross :: Mesh.Crossing -> BoundaryType
convertMeshCross Mesh.Face     = Face
convertMeshCross Mesh.Boundary = Escape
convertMeshCross Mesh.Self     = Reflect

-- | Convert a mesh Neighbor into an Event.
convertNeighbor :: Mesh.NeighborT SphericalMesh -> Event
convertNeighbor (Mesh.Neighbor _ face crossing) =
  BoundaryCross (convertMeshCross crossing) face


-- | A list of contractors that we hand to the step function.
contractors :: [Contractor]
contractors = [ timeStepContractor
              , meshContractor
              , scatteringContractor
              , absorptionContractor
              ]

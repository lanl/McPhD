{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SphericalApp.Tally where

import Data.Monoid
import qualified Data.Map as Map

import Utils.Combinators

import Mesh.Classes as Mesh
import Mesh.Spherical

import Space.Classes as Space
import Space.Spherical1D

import Properties
import SphericalApp.Physics
import SphericalApp.Events
import SphericalApp.Particle

-- * Aliases
type Momentum = Space.Velocity Spherical1D
type Space    = Spherical1D
type Cell     = Mesh.MeshCell SphericalMesh

-- * Tally data structures

-- | Information tallied in each cell.
data CellTally = CellTally !Momentum !Energy deriving (Eq, Show)

-- | Final events for particles, tallied globally
data EventCount = EventCount { nEscape  :: !Int
                             , nReflect :: !Int
                             , nTimeout :: !Int
                             } deriving (Show, Eq)

-- | The complete tally is the combination of all event contributions,
-- indexed by cell, plus the global counts.
data Tally = Tally { counts  :: EventCount
                   , perCell :: Map.Map Cell CellTally
                   }


-- * Combining events and tallies

-- | Add an event to the running tally.
addEvent :: Tally -> (Event, Particle) -> Tally
addEvent tally eAndP = tally <> eventToTally eAndP

-- | Convert an event into a mini-tally.
eventToTally :: (Event, Particle) -> Tally
eventToTally (event, Particle{cell=inCell}) =
  Tally (eventToCount event) (Map.singleton inCell $ eventToCellTally event )

-- | Convert an event into an EventCount
eventToCount :: Event -> EventCount
eventToCount (Boundary Escape _)  = EventCount 1 0 0
eventToCount (Boundary Reflect _) = EventCount 0 1 0
eventToCount Timeout              = EventCount 0 0 1
eventToCount Collide{}            = EventCount 0 0 0
eventToCount Boundary{}           = EventCount 0 0 0

-- | Convert an event into the corresponding CellTally
eventToCellTally :: Event -> CellTally
eventToCellTally Timeout    = mempty
eventToCellTally Boundary{} = mempty
eventToCellTally (Collide _ momentumDep energyDep) = CellTally momentumDep energyDep



-- * Monoid instance declarations

-- I'll eat my hat if there isn't a more idomatic way to do this.
-- | Monoid of Tallies
instance  Monoid Tally where
  mempty      = Tally { counts  = mempty, perCell = mempty }
  mappend s t = Tally { counts  = mappend (counts s)  (counts t)
                      , perCell = mappend (perCell s) (perCell t)
                      }
-- | Monoid CellTally
instance Monoid CellTally where
    mempty = CellTally 0 0
    mappend (CellTally m1 e1) (CellTally m2 e2) = CellTally (m1 + m2) (e1 + e2)

-- | Monoid EventCount
instance Monoid EventCount where
  mempty = EventCount 0 0 0
  mappend (EventCount ne1 nr1 nt1) (EventCount ne2 nr2 nt2) =
      EventCount (ne1+ne2) (nr1+nr2) (nt1+nt2)


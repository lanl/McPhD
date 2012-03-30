{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MiniApp.Tally where

import Data.Monoid
import qualified Data.Map as Map

import Utils.Combinators
import Mesh.Classes
import qualified Space.Classes as S

import Properties
import MiniApp.Physics
import MiniApp.Events
import MiniApp.Particle

-- * Aliases
type MomentumM m = S.Velocity (MeshSpace m)

-- * Tally data structures

-- | Information tallied in each cell.
data CellTally s = CellTally !(Momentum s) !Energy

-- | Final events for particles, tallied globally
data EventCount = EventCount { nEscape  :: !Int
                             , nReflect :: !Int
                             , nTimeout :: !Int
                             } deriving (Show, Eq)

-- | The complete tally is the combination of all event contributions,
-- indexed by cell, plus the global counts.
data Tally m = Tally { counts  :: EventCount
                     , perCell :: Map.Map (MeshCell m) (CellTally (MeshSpace m))
                     }



-- * Combining events and tallies

-- | Add an event to the running tally.
addEvent :: (Mesh m
            , Num (Momentum (MeshSpace m))) => Tally m -> (Event m, Particle m) -> Tally m
addEvent tally eAndP = tally <> eventToTally eAndP

-- | Convert an event into a mini-tally.
eventToTally :: (Mesh m, Num (Momentum (MeshSpace m))) => (Event m, Particle m) -> Tally m
eventToTally (event, Particle{cell=inCell}) =
  Tally (eventToCount event) (Map.singleton inCell $ eventToCellTally event )

-- | Convert an event into an EventCount
eventToCount :: (Mesh m) => Event m -> EventCount
eventToCount (Boundary Escape _)  = EventCount 1 0 0
eventToCount (Boundary Reflect _) = EventCount 0 1 0
eventToCount Timeout              = EventCount 0 0 1
eventToCount Collide{}            = EventCount 0 0 0
eventToCount Boundary{}           = EventCount 0 0 0

-- | Convert an event into the corresponding CellTally
eventToCellTally :: (Mesh m
                    , S.Space (MeshSpace m)
                    , Num (Momentum (MeshSpace m))) => Event m -> CellTally (MeshSpace m)
eventToCellTally Timeout    = mempty
eventToCellTally Boundary{} = mempty
eventToCellTally (Collide _ momentumDep energyDep) = CellTally momentumDep energyDep



-- * Monoid instance declarations

-- I'll eat my hat if there isn't a more idomatic way to do this.
-- | Monoid of Tallies
instance (Mesh m, Ord (MeshCell m)) => Monoid (Tally m) where
  mempty      = Tally { counts  = mempty, perCell = mempty }
  mappend s t = Tally { counts  = mappend (counts s)  (counts t)
                      , perCell = mappend (perCell s) (perCell t)
                      }
-- | Monoid CellTally
instance (Num (Momentum s), S.Space s) => Monoid (CellTally s) where
    mempty = CellTally 0 0
    mappend (CellTally m1 e1) (CellTally m2 e2) = CellTally (m1 + m2) (e1 + e2)

-- | Monoid EventCount
instance Monoid EventCount where
  mempty = EventCount 0 0 0
  mappend (EventCount ne1 nr1 nt1) (EventCount ne2 nr2 nt2) =
      EventCount (ne1+ne2) (nr1+nr2) (nt1+nt2)

-- | Show CellTally
deriving instance (S.Space s, Show (Momentum s)) => Show (CellTally s)

-- | Show Tally
deriving instance (Show (MeshCell m)
                  , S.Space (MeshSpace m)
                  , Num (Momentum (MeshSpace m))
                  , Show (MomentumM m)) => Show (Tally m)

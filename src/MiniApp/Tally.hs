{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MiniApp.Tally where
    
import Data.Monoid
import qualified Data.Map as Map

import Mesh.Classes
import qualified Space.Classes as Space

import Properties
import MiniApp.Physics
import MiniApp.Events
import MiniApp.Particle

-- * Tally data structures

-- | Information tallied in each cell.
data CellTally s = CellTally !(Momentum s) !Energy 

-- | Final events for particles, tallied globally
data EventCount = EventCount { nEscape  :: !Int 
                             , nReflect :: !Int 
                             , nTimeout :: !Int 
                             } deriving (Show, Eq)

-- | Events, plus per-cell tallies.
data Tally m = Tally { counts  :: EventCount
                     , perCell :: Map.Map (MeshCell m) (CellTally (MeshSpace m)) 
                     }
                                
data TallyContrib m = TallyContrib { contribCell   :: MeshCell m, 
                                     contribEffect :: CellTally m,
                                     contribCount  :: EventCount}

-- * Instance declarations 

-- | Show CellTally
deriving instance (Space.Space s, Show (Momentum s)) => Show (CellTally s)

-- | Monoid CellTally for folding tallies together
instance (Num (Momentum s), Space.Space s) => Monoid (CellTally s) where
    mempty = CellTally 0 0
    mappend (CellTally m1 e1) (CellTally m2 e2) = CellTally (m1 + m2) (e1 + e2)

-- | Monoid EventCount, for folding tallies together.
instance Monoid EventCount where
  mempty = EventCount 0 0 0
  mappend (EventCount ne1 nr1 nt1) (EventCount ne2 nr2 nt2) =
      EventCount (ne1+ne2) (nr1+nr2) (nt1+nt2)

-- | Show Tally
deriving instance (Show (MeshCell m), Space.Space (MeshSpace m), Show (Momentum (MeshSpace m))) => Show (Tally m)

-- I'll eat my hat if there isn't a more idomatic way to do this.
-- | Monoid of Tallies
instance (Mesh m, Ord (MeshCell m)) => Monoid (Tally m) where
  mempty      = Tally { counts  = mempty, perCell = mempty }
  mappend s t = Tally { counts  = mappend (counts s)  (counts t)
                      , perCell = mappend (perCell s) (perCell t) 
                      }

eventToCount :: (Mesh m) => Event m -> EventCount
eventToCount Timeout            = EventCount 0 0 1
eventToCount Collide{}          = EventCount 0 0 0
eventToCount (Boundary Escape _)  = EventCount 1 0 0
eventToCount (Boundary Reflect _) = EventCount 0 1 0
eventToCount (Boundary _ _)       = EventCount 0 0 0





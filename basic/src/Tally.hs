{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
module Tally where

import Control.DeepSeq
import Control.Monad
import Data.List as L
import Data.Monoid
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import Data.Vector.Unboxed as V

import Event
import Mesh
import Particle
import Physical

-- | Should and will be in Data.Monoid soon.
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
{-# INLINE (<>) #-}

data Tally = Tally {
    globalEvts :: !EventCount
  , deposition :: !PhysicsTally
  } deriving (Show)

instance NFData Tally

data CellTally = CellTally !Momentum !EnergyWeight
  deriving (Show)

newtype instance MVector s CellTally = MV_CellTally (MVector s (Momentum, EnergyWeight))
newtype instance Vector    CellTally = V_CellTally  (Vector    (Momentum, EnergyWeight))

instance GMV.MVector MVector CellTally where
  basicLength (MV_CellTally v) = GMV.basicLength v
  basicUnsafeSlice m n (MV_CellTally v) = MV_CellTally (GMV.basicUnsafeSlice m n v)
  basicOverlaps (MV_CellTally v1) (MV_CellTally v2) = GMV.basicOverlaps v1 v2
  basicUnsafeNew n = liftM MV_CellTally (GMV.basicUnsafeNew n)
  basicUnsafeRead (MV_CellTally v) n = GMV.basicUnsafeRead v n >>= \ (m, e) -> return (CellTally m e)
  basicUnsafeWrite (MV_CellTally v) n (CellTally m e) = GMV.basicUnsafeWrite v n (m, e)
instance GV.Vector   Vector  CellTally where
  basicLength (V_CellTally v) = GV.basicLength v
  basicUnsafeFreeze (MV_CellTally v) = liftM V_CellTally (GV.basicUnsafeFreeze v)
  basicUnsafeThaw (V_CellTally v) = liftM MV_CellTally (GV.basicUnsafeThaw v)
  basicUnsafeSlice m n (V_CellTally v) = V_CellTally (GV.basicUnsafeSlice m n v)
  basicUnsafeIndexM (V_CellTally v) n = GV.basicUnsafeIndexM v n >>= \ (m, e) -> return (CellTally m e)

instance Unbox CellTally

instance Monoid CellTally where
  mempty = CellTally 0 0
  mappend (CellTally m1 e1) (CellTally m2 e2) = CellTally (m1 + m2) (e1 + e2)

-- TODO: Using a vector here is somewhat risky. If there are many cells, we
-- will have to keep many sparse vectors around for intermediate results.
-- A finite map might be better at this stage.
type PhysicsTally = Vector CellTally

-- | Data structure for counting the number of occurrences of each of the
-- different event types.
data EventCount = EventCount {
    nScatter  :: !Int
  , nAbsorb   :: !Int
  , nTransmit :: !Int
  , nReflect  :: !Int
  , nEscape   :: !Int
  , nCensus   :: !Int
  } deriving (Show, Eq)

instance Monoid EventCount where
  mempty = EventCount 0 0 0 0 0 0
  mappend (EventCount s1 a1 t1 r1 e1 c1) (EventCount s2 a2 t2 r2 e2 c2) =
    EventCount (s1 + s2) (a1 + a2) (t1 + t2) (r1 + r2) (e1 + e2) (c1 + c2)

-- | Empty (initial) tally.
emptyTally :: Mesh m => m -> Tally
emptyTally msh = Tally mempty
                       (V.replicate (nrCells msh) (CellTally 0 0))

-- | Merge two tallies.
merge :: Tally -> Tally -> Tally
merge (Tally ec1 d1) (Tally ec2 d2) = Tally (ec1 <> ec2) (V.zipWith (<>) d1 d2)

-- TODO: It's a bit annoying that due to the use of vectors, we cannot
-- make Tally an instance of the Monoid class. Well, we could if we'd
-- reserve an additional constructor for an empty tally. But that's
-- probably not worth it.

-- | Tally all the particle states.
tally :: Mesh m => m -> [(Event, Particle)] -> Tally
tally msh = L.foldl' tallyImpl (emptyTally msh)

-- | Add the data of one event and particle to the current tally.
tallyImpl :: Tally -> (Event, Particle) -> Tally
tallyImpl (Tally ec d) (e, p) = Tally (countEvent e ec) (tDep e (cell p) d)

-- | Compute the deposition of a single event.
tDep :: Event -> CellIdx -> PhysicsTally -> PhysicsTally
tDep (Scatter _ dp e) (CellIdx cidx) t = accum (<>) t [(cidx, CellTally dp e)]
tDep (Absorb  _ dp e) (CellIdx cidx) t = accum (<>) t [(cidx, CellTally dp e)]
tDep _                _              t = t

-- TODO: It would be slightly cleaner, but potentially a bit less efficient,
-- to have a function computing a CellTally from an event, and always add that
-- to the PhysicsTally.

-- | Count a single event.
countEvent :: Event -> EventCount -> EventCount
countEvent (Scatter  {}) ctr = ctr { nScatter  = 1 + nScatter  ctr}
countEvent (Absorb   {}) ctr = ctr { nAbsorb   = 1 + nAbsorb   ctr}
countEvent (Transmit {}) ctr = ctr { nTransmit = 1 + nTransmit ctr}
countEvent (Escape   {}) ctr = ctr { nEscape   = 1 + nEscape   ctr}
countEvent (Reflect  {}) ctr = ctr { nReflect  = 1 + nReflect  ctr}
countEvent (Census   {}) ctr = ctr { nCensus   = 1 + nCensus   ctr}


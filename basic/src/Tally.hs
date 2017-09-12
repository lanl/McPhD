{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DeriveGeneric
  , TemplateHaskell #-}
module Tally
  where

import Control.DeepSeq
-- import Control.Monad
import Data.List as L
import Data.Monoid
-- import qualified Data.Vector.Generic as GV
-- import qualified Data.Vector.Generic.Mutable as GMV
import Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Deriving
import GHC.Generics (Generic)

import Event
import Mesh
import Particle
import Physical

-- | Data structure for counting the number of occurrences of each of the
-- different event types.
data EventCount = EventCount {
    nNuclAbs    :: !Int
  , nNuclEl     :: !Int
  , nEMinusInel :: !Int
  , nEPlusInel  :: !Int
  , nTransmit   :: !Int
  , nReflect    :: !Int
  , nEscape     :: !Int
  , nTimeout    :: !Int
  } deriving (Show, Eq, Generic)

instance NFData EventCount

data CellTally = CellTally !Momentum !Energy
  deriving (Show,Generic)

derivingUnbox "CellTally"
  [t| CellTally -> (Momentum,Energy) |]
  [| \(CellTally m e) -> (m,e) |]
  [| \(m,e) -> CellTally m e   |]

-- TODO: Using a vector here is somewhat risky. If there are many cells, we
-- will have to keep many sparse vectors around for intermediate results.
-- A finite map might be better at this stage.
type PhysicsTally = Vector CellTally

data Tally = Tally {
    globalEvts :: !EventCount
  , deposition :: !PhysicsTally
  , escape     :: ![(Energy,EnergyWeight)]
  } deriving (Show,Generic)

instance NFData Tally

instance Monoid CellTally where
  mempty = CellTally 0 0
  mappend (CellTally m1 e1) (CellTally m2 e2) = CellTally (m1 + m2) (e1 + e2)

instance Monoid EventCount where
  mempty = EventCount 0 0 0 0 0 0 0 0
  mappend (EventCount na1 ne1 emi1 epi1 t1 r1 e1 c1) (EventCount na2 ne2 emi2 epi2 t2 r2 e2 c2) =
    EventCount (na1 + na2) (ne1 + ne2) (emi1 + emi2) (epi1 + epi2) (t1 + t2) (r1 + r2) (e1 + e2) (c1 + c2)

type EscapeCount = [(Energy,EnergyWeight)]

-- | Empty (initial) tally.
emptyTally :: Mesh m => m -> Tally
emptyTally msh = Tally mempty
                       (V.replicate (nrCells msh) (CellTally 0 0))
                       mempty

-- | Merge two tallies.
merge :: Tally -> Tally -> Tally
merge (Tally ec1 d1 esc1) (Tally ec2 d2 esc2) =
  Tally (ec1 <> ec2) (V.zipWith (<>) d1 d2) (esc1 L.++ esc2)

-- | Tally all the particle states.
tally :: Mesh m => m -> [(Event, Particle)] -> Tally
tally msh = L.foldl' tallyImpl (emptyTally msh)

-- | Add the data of one event and particle to the current tally.
tallyImpl :: Tally -> (Event, Particle) -> Tally
tallyImpl (Tally ec dep esc) (evt, p) =
  Tally (countEvent evt ec) (tDep evt (cellIdx p) dep) (tEsc evt esc)

-- | Compute the deposition of a single event.
tDep :: Event -> CellIdx -> PhysicsTally -> PhysicsTally
tDep (Collision _ _ dp ed) (CellIdx cidx) tlly =
  accum (<>) tlly [(cidx, CellTally dp ed)]
tDep _                     _              tlly = tlly

-- tally an Escape event
tEsc :: Event -> EscapeCount -> EscapeCount
tEsc (Boundary Escape _ _ ed wt) ec = (ed,wt):ec
tEsc _ ec = ec

-- TODO: It would be slightly cleaner, but potentially a bit less efficient,
-- to have a function computing a CellTally from an event, and always add that
-- to the PhysicsTally.

-- | Count a single event.
countEvent :: Event -> EventCount -> EventCount

countEvent (Collision {cType = NuclEl})     ctr = ctr { nNuclEl = 1 + nNuclEl ctr}
countEvent (Collision {cType = NuclAbs})    ctr = ctr { nNuclAbs = 1 + nNuclAbs ctr}
countEvent (Collision {cType = EMinusInel}) ctr = ctr { nEMinusInel = 1 + nEMinusInel ctr}
countEvent (Collision {cType = EPlusInel})  ctr = ctr { nEPlusInel = 1 + nEPlusInel ctr}
countEvent (Boundary  {bType = Escape})   ctr = ctr { nEscape   = 1 + nEscape   ctr}
countEvent (Boundary  {bType = Reflect})  ctr = ctr { nReflect  = 1 + nReflect  ctr}
countEvent (Boundary  {bType = Transmit}) ctr = ctr { nTransmit  = 1 + nTransmit  ctr}
countEvent (Timeout   {}) ctr = ctr { nTimeout   = 1 + nTimeout   ctr}


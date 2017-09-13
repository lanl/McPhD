{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DeriveGeneric,
  TemplateHaskell #-}
module TallyV
  ( Tally(..)
  , CellTally(..)
  , EventCount(..)
  , tally
  , emptyTally
  , merge
  , totalDep
  , totalMCSteps
  )
  where

import Control.DeepSeq
import Data.List as L
import Data.Monoid (Monoid, mempty, mappend, (<>) )
import Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Deriving
import GHC.Generics (Generic)

import Event
import Mesh
import Particle
import Physical

data CellTally = CellTally {ctMom :: !Momentum, ctEnergy :: !Energy}
  deriving (Show,Eq, Generic)

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

derivingUnbox "CellTally"
  [t| CellTally -> (Momentum,Energy) |]
  [| \(CellTally m e) -> (m,e) |]
  [| \(m,e) -> CellTally m e |]

data Tally = Tally {
    globalEvts :: !EventCount
  , deposition :: !PhysicsTally
  , totalPL    :: Distance -- total path length
                           -- travelled by all particles
  } deriving (Show,Generic)

instance NFData Tally

-- TODO: Using a vector here is somewhat risky. If there are many cells, we
-- will have to keep many sparse vectors around for intermediate results.
-- A finite map might be better at this stage.
type PhysicsTally = V.Vector CellTally


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
                       {- mempty -}
                       0

-- | Merge two tallies.
merge :: Tally -> Tally -> Tally
merge (Tally ec1 d1 {- esc1 -} pl1) (Tally ec2 d2 {- esc2 -} pl2) =
  Tally (ec1 <> ec2) (V.zipWith (<>) d1 d2) {- (esc1 L.++ esc2) -} (pl1 + pl2)

-- TODO: It's a bit annoying that due to the use of vectors, we cannot
-- make Tally an instance of the Monoid class. Well, we could if we'd
-- reserve an additional constructor for an empty tally. But that's
-- probably not worth it.

-- | Tally all the particle states.
tally :: Mesh m => m -> [(Event, Particle)] -> Tally -> Tally
tally _ eps t = L.foldl' tallyImpl t eps

-- | Add the data of one event and particle to the current tally.
tallyImpl :: Tally -> (Event, Particle) -> Tally
tallyImpl (Tally ec dep {- esc -} pl) (evt, p) =
  Tally (countEvent evt ec) (tDep evt (cellIdx p) dep) {- (tEsc evt esc) -} (tPL evt pl)

-- | accumulate path length travelled to this event
tPL :: Event -> Distance -> Distance
tPL evt pl = pl + dist evt

-- | Compute the deposition of a single event.
tDep :: Event -> CellIdx -> PhysicsTally -> PhysicsTally
tDep (Collision _ _
                (Direction oli)
                (Energy ei)
                (Direction olf)
                (Energy ef)
                (EnergyWeight wt)) (CellIdx cidx) tlly =
  accum (<>) tlly [(cidx, CellTally pd ed)]
    where ed = Energy $   wt * (ei - ef)
          pd = Momentum $ wt / c * (ei * oli - ef * olf)

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
countEvent (Boundary  {bType = Escape})     ctr = ctr { nEscape   = 1 + nEscape   ctr}
countEvent (Boundary  {bType = Reflect})    ctr = ctr { nReflect  = 1 + nReflect  ctr}
countEvent (Boundary  {bType = Transmit})   ctr = ctr { nTransmit  = 1 + nTransmit  ctr}
countEvent (Timeout   {})                   ctr = ctr { nTimeout   = 1 + nTimeout   ctr}

totalMCSteps :: EventCount -> Int
totalMCSteps (EventCount na ne nem nep nt nr nesc nto) =
  na + ne + nem + nep + nt + nr + nesc + nto

totalDep :: Tally -> CellTally
totalDep t = V.foldl1' (<>)  (deposition t)

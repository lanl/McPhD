module Event where

import Data.List
import Data.Ord

import Cell
import Physical

-- | Events: what can happen to a particle on a Monte Carlo step.
--
-- Scatter:  some type of scatter; NB neutrions have more interaction
--           channels than photons: loks like ~6-8 per lepton, x2 for
--           anti-nu's
-- Absorb:   physically a subset of scattering, computationally different
--           in that absorption terminates a particle's flight
-- Transmit: reach a cell boundary and cross into a new mesh cell
-- Reflect:  reach a cell boundary and reflect from it
-- Escape:   reach a cell boundary and leave the problem domain
-- Census:   the particle's internal clock has reached the end of the
--           time step; it is banked for the next time step.

data Event =
    Scatter  { dist   :: !FP            -- ^ distance travelled
             , deltaP :: Momentum       -- ^ momentum deposited
             , eDep   :: EnergyWeight   -- ^ energy deposited
             }
  | Absorb   { dist   :: !FP            -- ^ distance travelled
             , deltaP :: Momentum       -- ^ momentum deposited
             , eDep   :: EnergyWeight   -- ^ energy deposited
             }
  | Transmit { dist   :: !FP            -- ^ distance travelled
             , face   :: Face           -- ^ which boundary
             }
  | Escape   { dist   :: !FP            -- ^ distance travelled
             , face   :: Face           -- ^ which boundary
             }
  | Reflect  { dist   :: !FP            -- ^ distance travelled
             , face   :: Face           -- ^ which boundary
             }
  | Census   { dist   :: !FP            -- ^ distance travelled
             , deltaP :: Momentum       -- ^ momentum deposited
             }
  deriving (Show, Eq)
-- REMARK: Only dist is strict. The reason is that we generate several
-- events in each turn, and only select the one with the smallest
-- distance. We don't actually want to perform the extra work for the
-- events that are discarded.

-- | Common type for all boundary event constructors.
type BoundaryEvent = FP -> Face -> Event

-- | Does an event continue a random walk?
isContinuing :: Event -> Bool
isContinuing (Scatter  {}) = True
isContinuing (Reflect  {}) = True
isContinuing (Transmit {}) = True
isContinuing _             = False

-- | Distance to be travelled for the event
distance :: Event -> FP
distance = dist

-- | Determines the closest event; input list must contain at least
-- one element.
closestEvent :: [Event] -> Event
closestEvent = minimumBy (comparing distance)


module Event where

import Data.List
import Data.Ord

import Cell
import Physical hiding (distance)

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


{-- QUESTION: In some sense, there are really three types here, along 
with two enumerations. The types are 
  Collision (NucleonAbsorb | ... | EPlusInelastic)
  Boundary (Transmit | Reflect | Escape )
  Census. 
Would the following make more sense?

-----
data CollType  = NucleonAbsorb | NucleonElastic | EMinusInelastic | EPlusInelastic

data BoundType = Transmit | Reflect | Escape

data Event = 
    Collision { dist   :: !Distance
              , deltaP :: Momentum
              , eDep   :: EnergyWeight
              , CollType
            }
  | Boundary { dist :: !Distance
             , face :: Face
             , BoundType
             }
  | Census   { dist :: !Distance
             , deltaP :: Momentum
             }
-----

I'm curious, especially in the case of Opacity.sampleCollision, whether there 
would be any benefit. 
-}
data Event =
    Scatter  { dist   :: !Distance            -- ^ distance travelled
             , deltaP :: Momentum       -- ^ momentum deposited
             , eDep   :: EnergyWeight   -- ^ energy deposited
             }
  | Absorb   { dist   :: !Distance            -- ^ distance travelled
             , deltaP :: Momentum       -- ^ momentum deposited
             , eDep   :: EnergyWeight   -- ^ energy deposited
             }
  | NucleonAbsorb  { dist   :: !Distance            -- ^ distance travelled
                   , deltaP :: Momentum       -- ^ momentum deposited
                   , eDep   :: EnergyWeight   -- ^ energy deposited
                   }
  | NucleonElastic { dist   :: !Distance            -- ^ distance travelled
                   , deltaP :: Momentum       -- ^ momentum deposited
                   , eDep   :: EnergyWeight   -- ^ energy deposited
                   }
  | EMinusInelastic { dist   :: !Distance            -- ^ distance travelled
                    , deltaP :: Momentum       -- ^ momentum deposited
                    , eDep   :: EnergyWeight   -- ^ energy deposited
                    }
  | EPlusInelastic  { dist   :: !Distance            -- ^ distance travelled
                    , deltaP :: Momentum       -- ^ momentum deposited
                    , eDep   :: EnergyWeight   -- ^ energy deposited
                    }
  | Transmit { dist   :: !Distance            -- ^ distance travelled
             , face   :: Face           -- ^ which boundary
             }
  | Escape   { dist   :: !Distance            -- ^ distance travelled
             , face   :: Face           -- ^ which boundary
             }
  | Reflect  { dist   :: !Distance            -- ^ distance travelled
             , face   :: Face           -- ^ which boundary
             }
  | Census   { dist   :: !Distance            -- ^ distance travelled
             , deltaP :: Momentum       -- ^ momentum deposited
             }
  deriving (Show, Eq)
-- REMARK: Only dist is strict. The reason is that we generate several
-- events in each turn, and only select the one with the smallest
-- distance. We don't actually want to perform the extra work for the
-- events that are discarded.

-- | Common type for all boundary event constructors.
type BoundaryEvent = Distance -> Face -> Event

-- | Does an event continue a random walk?
isContinuing :: Event -> Bool
isContinuing (Scatter  {}) = True
isContinuing (Reflect  {}) = True
isContinuing (Transmit {}) = True
isContinuing _             = False

-- | Distance to be travelled for the event
distance :: Event -> Distance
distance = dist

-- | Determines the closest event; input list must contain at least
-- one element.
closestEvent :: [Event] -> Event
closestEvent = minimumBy (comparing distance)


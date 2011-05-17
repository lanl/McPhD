module Event where

import Data.List
import Data.Ord

import Cell
import Physical hiding (distance)

-- | Events: what can happen to a particle on a Monte Carlo step.
-- Collision: interaction with the material medium. 
-- Boundary: Crossing a mesh boundary. 
-- Timeout: allows us to limit the time an individual particle is allowed to explore.

data CollType  = NuclAbs | NuclEl | EMinusInel | EPlusInel deriving (Show,Eq)

data BoundType = Transmit | Reflect | Escape deriving (Show,Eq)

data Event = 
    Collision { cType :: CollType
              , dist     :: !Distance   -- ^ distance travelled to collision
              , pDep     :: Momentum    -- ^ momentum transferred (k_i - k_f)
              , eDep     :: Energy      -- ^ energy transferred (E_i - E_f)
            }
  | Boundary { bType :: BoundType
             , dist      :: !Distance   -- ^ distance to boundary
             , face      :: Face        -- ^ which face intersected
             }
  | Timeout  { dist :: !Distance } -- ^ distance to timeout
  deriving (Show, Eq)

-- REMARK: Only dist is strict. The reason is that we generate several
-- events in each turn, and only select the one with the smallest
-- distance. We don't actually want to perform the extra work for the
-- events that are discarded.


-- | Common type for all boundary event constructors.
type BoundaryEvent = Distance -> Face -> Event

-- | Does an event continue a random walk?
isContinuing :: Event -> Bool
isContinuing (Collision {cType = NuclEl})     = True
isContinuing (Collision {cType = EMinusInel}) = True
isContinuing (Collision {cType = EPlusInel})  = True
isContinuing (Boundary {bType = Reflect})     = True
isContinuing (Boundary {bType = Transmit})    = True
isContinuing _                                = False

-- | Distance to be travelled for the event
distance :: Event -> Distance
distance = dist

-- | Determines the closest event; input list must contain at least
-- one element.
closestEvent :: [Event] -> Event
closestEvent = minimumBy (comparing distance)


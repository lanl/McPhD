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
    Collision { cType    :: !CollType
              , dist     :: !Distance   -- ^ distance travelled to collision
              , oInit    :: !Direction
              , eInit    :: !Energy
              , oFinal   :: !Direction
              , eFinal   :: !Energy
              , eWeight  :: !EnergyWeight
            }
  | Boundary { bType     :: !BoundType
             , dist      :: !Distance   -- ^ distance to boundary
             , face      :: !Face        -- ^ which face intersected
             , bEnergy   :: !Energy
             , bWeight   :: !EnergyWeight
             }
  | Timeout  { dist :: !Distance } -- ^ distance to timeout
  deriving (Show, Eq)

-- REMARK: Only dist is strict. The reason is that we generate several
-- events in each turn, and only select the one with the smallest
-- distance. We don't actually want to perform the extra work for the
-- events that are discarded.

-- | EventCandidate: useful for selecting the next event that
-- will befall a particle. Note: BoundaryCand carries the face
-- because that is a by-product of computing distance-to-boundary. 
data EventCandidate = 
    CollisionCand { candDist  :: !Distance }
  | BoundaryCand  { candDist  :: !Distance   
                  , bcFace    :: !Face        
                  }
  | TimeoutCand   { candDist  :: !Distance }
  deriving (Show, Eq)

-- | Common type for all boundary event constructors.
type BoundaryEvent = Distance -> Face -> Energy -> EnergyWeight -> Event

-- | Does an event continue a random walk?
isContinuing :: Event -> Bool
isContinuing (Collision {cType = NuclEl})     = True
isContinuing (Collision {cType = EMinusInel}) = True
isContinuing (Collision {cType = EPlusInel})  = True
isContinuing (Boundary {bType = Reflect})     = True
isContinuing (Boundary {bType = Transmit})    = True
isContinuing _                                = False

-- | Distance to be travelled for the event
distance :: EventCandidate -> Distance
distance = candDist

-- | Determines the closest event; input list must contain at least
-- one element.
closestEvent :: [EventCandidate] -> EventCandidate
closestEvent = minimumBy (comparing distance)


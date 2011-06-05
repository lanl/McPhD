module MiniApp.Outcome where
{-| A data type which stores an event, the next particle state, and
the distance to get there.

This is more general than the MiniApp, so it should be moved up somewhere.

-}

import Data.Function

import Mesh.Classes
import Properties

import MiniApp.Events
import MiniApp.Particle

-- | Outcomes are a Distance, Event and Particle which might happen
-- (candidates) or finally do happen.
-- TODO: Where should this definition go? It's more general than the MiniApp
data Outcome m = Outcome {
      distance :: !Distance
    , event    :: Event m
    , particle :: Particle m
    }

instance Mesh m => Eq (Outcome m) where
  (==) = (==) `on` distance

instance (Mesh m) => Ord (Outcome m) where
  (<=) = (<=) `on` distance

result :: Mesh m => Outcome m -> (Event m, Particle m)
result (Outcome _ event particle) = (event, particle)
module MonteCarlo where

import Data.Function

import Properties


-- | Outcomes are a distance to an event, the event and the next
-- particle state.
data Outcome e p = Outcome { distance :: Distance
                           , event    :: e
                           , particle :: p
                           }

instance Eq (Outcome e p) where
  (==) = (==) `on` distance

instance Ord (Outcome e p) where
  (<=) = (<=) `on` distance

-- | Convert an outcome into the (event, particle) result.
result :: Outcome ev part -> (ev, part)
result (Outcome _ event particle) = (event, particle)



-- | Contractors are functions which take a model, a particle and
-- return a candidate Outcome for stepping the particle.
type Contractor model particle event
    = model -> particle -> Outcome event particle

-- | Compute outcomes from contractors, and choose the closest one.
step :: model -> particle -> [Contractor model particle event] -> (event, particle)
step model particle contractors
    = result (minimum (map (\f -> f model particle) contractors))




-- | Stream a single particle:
stream :: (p -> (e,p))   -- ^ Function to produce each step. Comes from a model.
          -> (e -> Bool) -- ^ Check for terminal events to stop streaming
          -> p           -- ^ Initial particle
          -> [(e, p)]    -- ^ Resulting list of events and particle states.
stream stepper continue p = next p
  where next p =
          let (e, p') = stepper p
          in  (e, p') : if continue e then next p' else []

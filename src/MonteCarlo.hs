module MonteCarlo where
{- | Take application-specific stepping functions, particle event and tally
data and combine them into a Monte Carlo simulation.
-}

import Data.Function

import Properties


-- ??? Better to make these function independent of the model? Then
-- the application would provide functions like: Particle -> ...
-- instead of Model -> Particle -> ...  It would likely do this by
-- defining functions like Model -> Particle -> ... and provide
-- closures of them by evaluating them over the model.
--
-- They could even just provide functions which use a model object
-- defined in the miniApp module, to avoid the need for closures.
--
-- It just seems kind of odd to always be passing Model and Particle
-- to these higher-level operators when only the particle changes over
-- the course of the simulation. We don't really need to know about
-- the model at this level.


-- | Outcomes are a distance to an event, the event and the next
-- particle state.
data Outcome e p = Outcome { distance :: !Distance -- ^ Strict, becuase we use it to select winners.
                           , event    :: e
                           , particle :: p
                           }

-- We compare outcomes strictly on the basis of distance.
instance Eq (Outcome e p) where
  (==) = (==) `on` distance

instance Ord (Outcome e p) where
  compare = compare `on` distance

-- | Convert an outcome into the (event, particle) result.
result :: Outcome ev part -> (ev, part)
result (Outcome _ event particle) = (event, particle)


-- ??? Just use Outcome instead of (event, particle) elsewhere?  This
-- doesn't seem any more restritive than requiring apps to use the
-- Contractor type and (e,p). The higher level functions can still be
-- written polymorphicaly over the tally, particle and outcome types.


-- | Contractors are functions which take a model, a particle and
-- return a candidate Outcome for stepping the particle.
type Contractor model particle event
    = model -> particle -> Outcome event particle

-- | Compute outcomes from contractors, and choose the closest one.
step :: model 
     -> [Contractor model particle event] 
     -> particle 
     -> (event, particle)
step model contractors particle
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

executeMC :: (p->o) -> (o->t) -> (t->t->t) -> [p] -> t
executeMC stream collapse combine initial =
    foldl1 combine (map (collapse . stream) initial)

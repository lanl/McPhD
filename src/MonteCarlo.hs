module MonteCarlo where

-- These functions seem pretty general. They would use data types
-- defined in each application:

-- | Stream a single particle:
stream :: (p -> (e,p))   -- ^ Function to produce each step. Comes from a model.
          -> (e -> Bool) -- ^ Check for terminal events to stop streaming
          -> p           -- ^ Initial particle
          -> [(e, p)]    -- ^ Resulting list of events and particle states.
stream stepper continue p = next p
  where next p =
          let (e, p') = stepper p
          in  (e, p') : if continue e then next p' else []


-- | TODO: A step function which evaulautes competing events for particle motion



-- * These functions probably need to be part of a specific application.

-- | Create a Model and Source from a problem specification.
-- Spec -> (Model, Source)

-- | Create an Problem specification from an Input source.
-- | filename -> IO (Spec)

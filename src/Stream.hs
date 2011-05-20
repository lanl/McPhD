{-# LANGUAGE TypeFamilies #-}

module Stream where

-- | Stream a particle until it terminates.
stream :: (p -> (e,p))   -- ^ Function to produce each step.
          -> (e -> Bool) -- ^ Check for terminal events to stop streaming
          -> p           -- ^ Initial particle
          -> [(e, p)]    -- ^ Resulting list of events and particle states.
stream stepper continue p = next p
  where next p = 
          let (e, p') = stepper p
          in  (e, p') : if continue e then next p' else []
            

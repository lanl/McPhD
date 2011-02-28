{-# LANGUAGE TypeFamilies #-}

module Stream where

import Data.List

-- Make a plain step function from context and the get environment
-- function
addEnv :: con                        -- ^ A context
	  -> (con -> p -> env)       -- ^ Generate environment from context
	  -> (env -> p -> (evnt, p)) -- ^ The particle step function
	  -> p                       -- ^ Particle
	  -> (evnt , p)              -- ^ Resulting event and particle
addEnv con env_fn step_fn p =
  let get_env = env_fn con -- ^ closure for environment function
  in step_fn (get_env p) p


-- Attach a Done state to the particle which still carries the
-- particle. This way we can still get the last one.
data Status p = Streaming p | Done p deriving (Show)


-- Transform a particle step function + end tester into the maybe form.
addStat :: (p -> (evnt, p))          -- ^ Plain step function
	   -> (evnt -> Bool)         -- ^ Test for final
	   -> Status p               -- ^ Particle with status
	   -> Maybe (evnt, Status p) -- ^ Maybe an event and new particle
addStat _ _ (Done p) = Nothing  -- ^ Convert done particles to Nothing.
addStat step final (Streaming p)
  = let (e, p') = step p
    in if (final e)
       then Just (e, Done p')
       else Just (e, Streaming p')


stream :: con                        -- ^ A context
	  -> (con -> p -> env)       -- ^ Generate environment from context
	  -> (env -> p -> (evnt, p)) -- ^ The particle step function
	  -> (evnt -> Bool)          -- ^ Test for final events
	  -> p                       -- ^ An initial particle
	  -> [evnt]                  -- ^ Resulting events
stream context get_env_fn step_fn final_fn p =
  let step_env_fn = addEnv  context get_env_fn step_fn
      step_status = addStat step_env_fn final_fn
  in unfoldr step_status (Streaming p)

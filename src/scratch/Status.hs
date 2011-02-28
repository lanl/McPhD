import Data.List


-- A simple particle and stepping function for testing.

data Environment = Environment { limitEnv :: Int } deriving (Show)
data Context     = Context     { limitCon :: Int } deriving (Show)
data Particle    = Particle    { count :: Int } deriving (Show)
data Event = Incremented Int | Stopped deriving (Show)

step :: Environment -> Particle -> (Event, Particle)
step e p = if (count p < limitEnv e)
	   then (Incremented (count p), inc p)
	   else (Stopped, p)

inc :: Particle -> Particle
inc p = p { count = (count p)+1 }

-- A test for events which are final
is_final :: Event -> Bool
is_final Stopped = True
is_final _       = False


-- Context -> Environment map
get_env :: Context -> Particle -> Environment
get_env (Context limit) _  = Environment limit



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

-- Make a plain step function from context and the get environment
-- function
addEnv :: con -- ^ A context
	  -> (con -> p -> env)       -- ^ Generate environment from context
	  -> (env -> p -> (evnt, p)) -- ^ The particle step function
	  -> p                       -- ^ Particle
	  -> (evnt , p)              -- ^ Resulting event and particle
addEnv con env_fn step_fn p =
  let get_env = env_fn con -- ^ closure for environment function
  in step_fn (get_env p) p



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


tally :: (env -> tp)        -- ^ Map events into tally parts
	 -> (tp -> t -> t)  -- ^ Combine a tally part with a tally
	 -> t               -- ^ Initial tally
	 -> [env]           -- ^ Events to tally
	 -> t               -- ^ Resulting tally

tally contrib combine init events =
  let ev_combine = contrib . combine
  in foldr ev_combine init




main :: IO ()
main =
  let particle = Particle 0
      context  = Context 3
      events   = stream context get_env step is_final particle
  in putStrLn $ show events

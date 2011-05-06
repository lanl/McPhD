module Primer where

import Space
import Data.List
import Data.Vector.V3
-- We simlulate movement of Particles in some kind of environment

-- So lets come up with some particles:
data Particle =
  NoParticle -- there is no particle :) Could happen in simulation when particle disintegrates
  | StillParticle { spPos :: Position } -- Particle that stays put
  | DetParticle { pPos :: Position -- deterministic particle
                , pDir :: Direction
                , pLim :: Maybe Distance -- limit on how far it could travel
                } 
  | RandomParticle { thePart::Particle, randState :: Double } -- non-det particle with random state (just a mock-up)
  | FinalParticleState Particle -- ^ Syntetic particle to record final particle state in simulation
  deriving Show
           
-- And some environments:
data Environment =
  EmptySpace -- particles travel unchecked
  | Cube { farCorner :: Position } -- cube from (0,0,0) to the farCorner
  -- add meshes, etc later
  deriving Show
           
-- Now, simulation is just a series of steps, each one producing the event and new particle state.
-- What are the events?
data Event = Movement Direction Position -- ^ Some movement occured, final position given
           | Boundary Position -- ^ Some boundary was hit at given position
           | Scatter Position -- ^ Scattering occured there
           | Escape Position -- ^ Particle escaped comp. domain at position 
           | Absorption Position -- ^ Particle has been absorbed into the material.
           | FinalState Particle -- ^ Syntetic event to record final particle state
           deriving Show
                    
-- Which events are considered final?
isFinal FinalState{} = True
isFinal _ = False

-- Now we could define simulation as the unfolding of steps, until final event was reached.
-- We are interested in final state of the particle
simulate :: Environment -> Particle  -> [Event]
simulate env p = unfoldr (step env) p

-- Simulate till final step is reached, extract particle state
simulateTillFinal :: Environment -> Particle  -> ([Event], Particle)
simulateTillFinal env p = 
  let (track, (FinalState fs):_) = span (not.isFinal) $ simulate env p
      in (track, fs)

-- What happens in the simulation step depends on the particle and environment
step :: Environment -> Particle -> Maybe (Event, Particle)

-- If we have no particle, simulation terminates
step _ NoParticle = Nothing 

-- If we have particle in its final state, let's transmit it to the event
step _ (FinalParticleState p) = Just (FinalState p, NoParticle)

-- Now, temination conditions are set and we are ready to simulate some particles
-- StillParticle is not going anywhere is any environment
step _ p@(StillParticle{}) = Just (Movement (Direction (Vector3 0 0 0)) (spPos p), p)

-- Unconstrained DetParticle in EmptySpace is convorming to the 1st law of Newton
step EmptySpace part@(DetParticle pos dir Nothing) = 
  let pos' = translate pos dir (Distance 1) in Just (Movement dir pos', part{pPos = pos'})
                                               
-- Constrained DetParticle travels until the limit is hit, then it is scattered (just a mock-up, again)
step EmptySpace part@(DetParticle pos dir (Just lim)) = 
  if lim == 0 
  then Just (Scatter pos, FinalParticleState part)
  else 
    let lim' = max (lim - 1) 0
        pos' = translate pos dir (Distance 1) 
    in Just (Movement dir pos', part{pPos = pos', pLim = Just lim'})

step env part = error $ "Don't know how to simulate " ++ show part ++ " in environment " ++ show env

---
-- Now, let's play
---
main = do
  putStrLn "StillParticle in EmptySpace, 10 steps"
  mapM_ print $ take 10 $ simulate EmptySpace (StillParticle (Position (Vector3 1 2 3)))
  
  putStrLn "\n>>> DetParticle without limit in EmptySpace, 10 steps"
  mapM_ print $ take 10 $ simulate EmptySpace (DetParticle (Position (Vector3 1 1 1)) (Direction (Vector3 1 1 1)) Nothing)
  
  putStrLn "\n>>> DetParticle with limit 5 in EmptySpace, 10 steps"
  mapM_ print $ take 10 $ simulate EmptySpace (DetParticle (Position (Vector3 1 1 1)) (Direction (Vector3 1 1 1)) (Just 5))
  
  putStrLn "\n>>> last case, but via simulateTillFinal"
  print $ simulateTillFinal EmptySpace (DetParticle (Position (Vector3 1 1 1)) (Direction (Vector3 1 1 1)) (Just 5))

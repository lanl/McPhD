module MC
  ( module MC
  , module Cell
  , module Event
  , module P
  , module Tally
  , module Material
  )
  where

import Control.Applicative
import Cell
import Event
import Mesh as M
import Particle as P
import Physical
import Philo2
import Tally
import Sigma_HBFC
import Collision
import Material

-- import Debug.Trace

-- | Run an individual particle in a mesh. Returns the tally containing
-- information about all the generated events.
runParticle :: Mesh m => Lepton -> m -> Particle -> RNG -> Tally
runParticle sig msh p g0 = tally msh $ steps sig msh p g0

-- | Compute the trace of a particle as long as events that allow continuing
-- are produced. Returns the entire list.
stepsPs :: Mesh m => Lepton -> m -> [Particle] -> [RNG] -> [(Event, Particle)]
-- stepsPs sig msh ps g0 = trace (repChunk evts g0) evts
stepsPs sig msh ps gs = evts
  where
    evts = concat $ zipWith go gs ps
    go g0 p = let (evt, p') = step sig msh p g0
                  g1 = 
                       incrRNG . incrRNG $ g0
              in -- trace ("Event: " ++ show evt ++ "\ninit particle: " ++
                 --        show p ++ "\ngen: " ++ show g0) $
                   ((evt, p') :) $ if isContinuing evt then go g1 p' else []

-- | Compute the trace of a particle as long as events that allow continuing
-- are produced. Returns the entire list.
steps :: Mesh m => Lepton -> m -> Particle -> RNG -> [(Event, Particle)]
steps sig msh = go
  where
    go p g0 =
      let (evt, p') = step sig msh p g0
          g1 = incrRNG . incrRNG $ g0
      in ((evt, p') :) $ if isContinuing evt then go p' g1 else []

-- | Compute the next event for a given particle in a given mesh. Also
-- returns the new state of the particle. The flow is as follows:
--   pickEvent:    selects one candidate event
--   stream:       updates the particle to the event location
--   processEvent: generates the final particle state and the event record.
step :: Mesh m => Lepton-> m -> Particle -> RNG -> (Event, Particle)
step sig msh p g0 = let
  (sel_d_coll,_) = random2 g0
  g1 = incrRNG g0
  (sel_coll,sel_newState) = random2 g1
  
  evtCand = pickEvent sig msh p sel_d_coll
  pInit   = stream msh p evtCand
  in processEvent evtCand msh sig pInit sel_coll sel_newState

-- | Turn an event candidate and an input particle into an event and an 
-- output particle. 
processEvent :: Mesh m => EventCandidate -> m -> Lepton -> Particle ->
                URD -> URD ->
                (Event,Particle)
processEvent (TimeoutCand td) _ _ p _ _ = (Timeout td, p{time = Time 0})
processEvent (BoundaryCand dBdy fce) msh _ p@(Particle { P.dir   = o
                                                       , energy  = nrg
                                                       , weight  = wt
                                                       , cellIdx = cidx
                                                       }) _ _ = 
  case boundaryEvent msh cidx dBdy fce nrg wt of
    b@(Boundary {bType = Reflect})  -> (b, p{P.dir = -o})
    b@(Boundary {bType = Escape})   -> (b, p{P.cellIdx = -1})
    b@(Boundary {bType = Transmit}) ->
         let newCIdx = cellAcross msh cidx fce
         in  (b, p{P.cellIdx = newCIdx})
processEvent (CollisionCand dCol) msh sig p@(Particle { P.dir   = o
                                                      , energy  = nrg
                                                      , weight  = wt
                                                      , cellIdx = cidx
                                                      }) xi1 xi2 =
  let evt = sampleCollision msh (M.cell msh cidx) nrg o sig dCol wt xi1 xi2
      o'  = oFinal evt
      e'  = eFinal evt
  in (evt, p{ P.dir = o', energy = e'})

-- | Computes a new particle that represents the input Particle
-- moved to the Event location. Nothing else about the particle
-- is updated--cell index, for example, is unchanged.
stream :: Mesh m => m -> Particle -> EventCandidate -> Particle
stream msh
       p@(Particle { P.dir = omega
                   , P.pos = r
                   , time  = tm
                   })
       cand = p {P.pos = r',P.dir=o',time=t'}
         where (r',o') = newCoord msh r omega d
               t'      = tm - Time (Physical.distance d / c)
               d       = candDist cand

-- | Determine the next event that occurs for a particle, and
-- a new state for the particle.
pickEvent :: Mesh m => Lepton -> m -> Particle -> URD -> EventCandidate
pickEvent sig msh 
          Particle { P.dir     = omega
                   , P.pos     = r
                   , cellIdx   = cidx
                   , time      = Time tcen
                   , energy    = nrg
                   }
          sel_dc
          =
  let (dBdy, fce) = distanceToBoundary msh mcell r omega
      dCol        = dCollide mcell nrg omega sig sel_dc
      dCen        = Distance $ c * tcen
      mcell       = M.cell msh cidx
      eventCands  = [ CollisionCand dCol
                    , BoundaryCand  dBdy fce
                    , TimeoutCand   dCen
                    ]

  in closestEvent eventCands

-- the end


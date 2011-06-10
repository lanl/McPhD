module MC where

import Cell
import Event
import Mesh as M
import Particle as P
import Physical
import PRNG
import Tally
import Sigma_HBFC
import Collision

-- | Run an individual particle in a mesh. Returns the tally containing
-- information about all the generated events.
runParticle :: Mesh m => Lepton -> m -> Particle -> Tally
runParticle sig msh p = tally msh (steps sig msh p)

-- | Compute the trace of a particle as long as events that allow continuing
-- are produced. Returns the entire list.
steps :: Mesh m => Lepton -> m -> Particle -> [(Event, Particle)]
steps sig msh = go
  where
    go p = case step sig msh p of
             (e, p') -> (e, p') : if isContinuing e then go p' else []

-- | Compute the next event for a given particle in a given mesh. Also
-- returns the new state of the particle.
step :: Mesh m => Lepton-> m -> Particle -> (Event, Particle)
step sig msh p =
  withRandomParticle p $ do
    (evt,omega') <- pickEvent sig msh p 
    let p'       =  stream msh p evt omega'
    return (evt, p')

-- | Computes a new particle from a particle, a direction and
-- an event.
stream :: Mesh m => m -> Particle -> Event -> Direction -> Particle
stream msh
       p@(Particle { P.dir = omega
                   , P.pos = (Position x)
                   , time  = t
                   , cellIdx  = cidx
                   })
       event omega' =
  case event of
    Collision {}                 -> p' { P.dir =  omega'   }
    Boundary  {bType = Reflect}  -> p' { P.dir = -omega    }
    Boundary  {bType = Transmit} -> p' { cellIdx  = newCell f }
    Boundary  {bType = Escape}   -> p' { cellIdx  = 0         } 
    Timeout   {}                 -> p' { time  = 0         }
  where
    p' :: Particle
    p' = p { P.pos = x', time = t' }
    d  :: Distance
    d  = dist event
    x' :: Position
    x' = Position $ x +  (Physical.dir omega) * Physical.distance d
    t' = t - Time (Physical.distance d / c)
    f = face event
    newCell :: Face -> CellIdx
    newCell = cellAcross msh cidx

-- | Wrap a random computation based on a particle. Extracts the
-- random number generator from the particle in the beginning, and
-- put it back in at the end of the computation.
withRandomParticle :: Particle -> Rnd (a, Particle) -> (a, Particle)
withRandomParticle (Particle { rng = rng }) m =
  case runRnd rng m of
    ((x, p), rng') -> (x, p { rng = rng' })

-- | Determine the next event that occurs for a particle, and
-- a new state for the particle.
pickEvent :: Mesh m => Lepton -> m -> Particle -> Rnd (Event,Direction)
pickEvent sig msh
          Particle { P.dir     = omega
                   , P.pos     = x
                   , cellIdx   = cidx
                   , weight    = wt
                   , time      = Time tcen
                   , energy    = e
                   }
          = do
  sel_dc <- random
  let (dBdy, face) = distanceToBoundary msh mcell x omega
      dCol         = dCollide mcell e omega sig (URD sel_dc)
      dCen         = Distance $ c * tcen
      mcell        = M.cell msh cidx 
  (collEvent,omega') <- sampleCollision msh mcell e omega sig dCol wt
  let events        = [
          collEvent
        , boundaryEvent msh cidx dBdy face e wt
        , Timeout dCen
        ]

  return ((closestEvent events),omega')

-- the end


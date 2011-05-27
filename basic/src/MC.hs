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
step sig msh p@(Particle {cellIdx = cidx
                         ,P.dir  = o
                         ,energy  = e}) =
  withRandomParticle p $ do
    omega' <- sampleDirectionIso msh (M.cell msh cidx) e o 
    evt    <- pickEvent sig msh p omega'
    let p' =  stream msh p omega' evt
    return (evt, p')

-- | Computes a new particle from a particle, a direction and
-- an event.
stream :: Mesh m => m -> Particle -> Direction -> Event -> Particle
stream msh
       p@(Particle { P.dir = omega
                   , P.pos = (Position x)
                   , time  = t
                   , cellIdx  = cidx
                   })
       omega' event =
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
pickEvent :: Mesh m => Lepton -> m -> Particle -> Direction -> Rnd Event
pickEvent sig msh
          Particle { P.dir     = omega
                   , P.pos     = x
                   , cellIdx   = cidx
                   , weight    = wt@(EnergyWeight w)
                   , time      = Time tcen
                   , energy    = e@(Energy nrg)
                   }
          omega' = do
  sel_dc <- random
  sel_sc <- random
  let (dBdy, face) = distanceToBoundary msh mcell x omega
      dCol         = dCollide mcell e omega sig (URD sel_dc)
      dCen         = Distance $ c * tcen
      mcell        = M.cell msh cidx
 
      dp :: Direction -> Momentum
      dp = elasticScatterMomDep e omega

      -- Events to consider:
      collEvent = sampleCollision mcell e omega sig (URD sel_sc)
      events        = [
          collEvent dCol (dp omega') (Energy $ nrg * w)
        , boundaryEvent msh cidx dBdy face e wt
        , Timeout dCen
        ]

  return (closestEvent events)

-- | Compute the elastic scattering momentum deposition:
-- E/c (k_i^ - k_f)
elasticScatterMomDep :: Energy -> Direction -> Direction -> Momentum
elasticScatterMomDep (Energy e) (Direction omega_i) (Direction omega_f) =
  Momentum ((e / c) * (omega_i - omega_f))


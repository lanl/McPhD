module MC where

import Cell
import Event
import Material
import Mesh
import Particle as P
import Physical
import PRNG
import Tally

-- | Run an individual particle in a mesh. Returns the tally containing
-- information about all the generated events.
runParticle :: Mesh m => m -> Particle -> Tally
runParticle msh p = tally msh (steps msh p)

-- | Compute the trace of a particle as long as events that allow continuing
-- are produced. Returns the entire list.
steps :: Mesh m => m -> Particle -> [(Event, Particle)]
steps msh = go
  where
    go p = case step msh p of
             (e, p') -> (e, p') : if isContinuing e then go p' else []

-- QUESTION: So currently, we do not include the original state of the
-- particle in the tally, but we include the final state (i.e., the one
-- *after* we've already decided not to continue. Is this the correct
-- choice?

-- | Compute the next event for a given particle in a given mesh. Also
-- returns the new state of the particle.
step :: Mesh m => m -> Particle -> (Event, Particle)
step msh p =
  withRandomParticle p $ do
    omega' <- sampleDirection msh
    evt    <- pickEvent msh p omega'
    let p' =  stream msh p omega' evt
    return (evt, p')

-- | Computes a new particle from a particle, a direction and
-- an event.
stream :: Mesh m => m -> Particle -> Direction -> Event -> Particle
stream msh
       p@(Particle { P.dir = omega
                   , P.pos = (Position x)
                   , time  = t
                   , cell  = cidx
                   })
       omega' event =
  case event of
    Scatter  {}           -> p' { P.dir =  omega'   }
    Absorb   {}           -> p' { P.dir =  omega    }
    Reflect  {}           -> p' { P.dir = -omega    }
    Transmit { face = f } -> p' { cell  = newCell f }
    Escape   {}           -> p' { cell  = 0         } -- QUESTION: 0?
    Census   {}           -> p' { time  = 0         }
  where
    p' :: Particle
    p' = p { P.pos = x', time = t' }
    d  :: FP
    d  = dist event
    x' :: Position
    x' = Position $ x +  (Physical.dir omega) * d
    t' = t - Time (d / c)

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
pickEvent :: Mesh m => m -> Particle -> Direction -> Rnd Event
pickEvent msh
          Particle { P.dir  = omega
                      , P.pos  = x
                      , cell   = cidx
                      , weight = w
                      , time   = Time tcen
                      , energy = e
                      }
          omega' = do
  sel_s <- random
  sel_a <- random
  let (d_bdy, face) = distanceToBoundary msh cidx x omega
      matl          = material msh cidx
      sig_s         = sigma $ sig_scat matl
      sig_a         = sigma $ sig_abs  matl
      -- TODO: boost to co-moving frame, compute the scatter, boost
      -- back to lab frame
      d_scat        = min (- log sel_s / sig_s) huge
      d_abs         = min (- log sel_a / sig_a) huge
      d_cen         = c * tcen

      dp :: Direction -> Momentum
      dp = elasticScatterMomDep e omega

      -- Events to consider:
      events        = [
          Scatter       d_scat (dp omega') w
        , Absorb        d_abs  (dp 0)      w
        , boundaryEvent msh cidx d_bdy face
        , Census        d_cen  0
        ]

  return (closestEvent events)

-- | Compute the elastic scattering momentum deposition:
-- E/c (k_i^ - k_f)
elasticScatterMomDep :: Energy -> Direction -> Direction -> Momentum
elasticScatterMomDep (Energy e) (Direction omega_i) (Direction omega_f) =
  Momentum ((e / c) * (omega_i - omega_f))


module Source where

import Control.Monad.State
import Data.List

import Mesh
import Particle
import Physical
import PRNG

-- | Generate a number of random particles.
genParticles :: Mesh m => Int -> m -> RNG -> [Particle]
genParticles n msh rng =
  let rngs = take n (unfoldr (Just . split) rng)
  in  map (genParticle msh) rngs

-- | Generate a single random particle.
genParticle :: Mesh m => m -> RNG -> Particle
genParticle msh rng =
  fst $ runRnd rng $ do
    (x, c) <- samplePosition  msh
    d      <- sampleDirection msh
    let t  :: Time
        t  = 1
        e  :: Energy
        e  = 1
        ew :: EnergyWeight
        ew = 1
    rng'   <- get
    return (Particle x d t e ew c rng')

-- | generate a given number of particles in each cell
genParticlesInCells :: Mesh m => m -> RNG -> [(CellIdx,Int)] -> [Particle]
genParticlesInCells msh rng nPerCell = 
  join $ map (genParticlesInCell msh rng) nPerCell

-- | generate a given number of particles in one cell
genParticlesInCell :: Mesh m => m -> RNG -> (CellIdx,Int) -> [Particle]
genParticlesInCell msh rng (cidx,n) =
  let rngs = take n (unfoldr (Just . split) rng)
  in  map (genCellParticle msh cidx) rngs

-- | Generate a single random particle.
genCellParticle :: Mesh m => m -> CellIdx -> RNG -> Particle
genCellParticle msh cidx rng =
  fst $ runRnd rng $ do
    let cll = cell msh cidx
    d <- sampleDirection msh
    x <- samplePositionInCell msh cll
    let 
        t  :: Time
        t  = 1
        e  :: Energy
        e  = 1
        ew :: EnergyWeight
        ew = 1
    rng'   <- get
    return (Particle x d t e ew cidx rng')

-- 

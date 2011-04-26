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

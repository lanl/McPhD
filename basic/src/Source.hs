module Source where

import Control.Monad.State
import Control.Applicative ()
import Data.List

import Mesh
import Particle
import Physical
import PRNG
import Geometry
import Material
import Cell

-- * particle generation

-- | generate a given number of particles in each cell
genParticlesInCells :: Mesh m => m -> RNG -> [(CellIdx,Int)] -> 
                       FP ->  -- ^ alpha (power law parameters)
                       FP ->  -- ^ e'
                      [Particle]
genParticlesInCells msh rng nPerCell a e' = 
  join $ map (genParticlesInCell msh rng a e') nPerCell

-- | generate a given number of particles in one cell
genParticlesInCell :: Mesh m => m -> RNG ->
                      FP ->  -- ^ alpha (power law parameters) 
                      FP ->  -- ^ e'
                     (CellIdx,Int) ->
                     [Particle]
genParticlesInCell msh rng a e' (cidx,n) =
  let rngs = take n (unfoldr (Just . split) rng)
  in  map (genCellParticle msh cidx a e') rngs

-- | Generate a single random particle.
genCellParticle :: Mesh m => m -> CellIdx -> 
                   FP ->  -- ^ alpha  (power law parameters)
                   FP ->  -- ^ e' (must be in comoving frame!)
                   RNG -> 
                   Particle
genCellParticle msh cidx a e' rng =
  fst $ runRnd rng $ do
    let cll = cell msh cidx
    -- to do: sample energy, then sample direction 
    -- using Collision.sampleDirectionIso
    oc <- sampleDirectionIso msh 
    x <- samplePositionInCell msh cll
    de <- samplePowerLaw a e'  
    let 
        t  :: Time
        t  = 1
        ec  = Energy de
        ew :: EnergyWeight
        ew = 1
        (e,o) = comovingToLab ec oc (mvel $ mat cll)
    rng'   <- get
    -- LT from comoving frame to lab frame
    return (Particle x o t e ew cidx rng')

-- * energy sampling

-- | sample energy from power-law approximation to LTE Fermion
-- distribution, as per Keil et al. Ap J 2003. The power law is
--   f_a(e) = (e/e')^a * Exp[-(a+1)*e/e']
-- where a is a parameter dictated by neutrino degeneracy and 
-- e' is the mean energy.
-- We use Aimee Hungerford's rejection method modified to sample a
-- straight exponential and reject using g(y) = y^a Exp(-ay)/Exp(-a).
samplePowerLaw :: FP -> FP -> Rnd FP
samplePowerLaw a e' = do
  y        <- random
  selector <- random
  let x = - log y
  if selector <= rejector x a 
  then return $ x * e' 
  else samplePowerLaw a e'

-- | power law rejection function 
rejector :: FP -> FP -> FP
rejector x a = x**a * (exp (-a*x)) / exp (-a)


-- * source statistics

-- | Compute energy emitted in cell volume from luminosity in each cell
evolFromLum :: [Luminosity] -> Time -> [Energy]
evolFromLum ls (Time dt) = [Energy $ dt * l |(Luminosity l)<-ls]

-- | Compute number of particles emitted in each cell 
srcNumsFromLum :: [Energy] -> Int -> [Int]
srcNumsFromLum ePerCell nTot = nPerCell
  where nPerCell = [round $ nt * frac  | frac <- fracs] :: [Int]
        fracs    = [ eC/eTot | (Energy eC) <- ePerCell]
        Energy eTot = sum ePerCell
        nt :: Double
        nt = fromIntegral nTot 


-- * older generation routine, useful for testing

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
    d      <- sampleDirectionIso msh
    let t  :: Time
        t  = 1
        e  :: Energy
        e  = 1
        ew :: EnergyWeight
        ew = 1
    rng'   <- get
    return (Particle x d t e ew c rng')

-- 

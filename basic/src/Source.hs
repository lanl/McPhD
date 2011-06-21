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

type SrcStat = (CellIdx,Int,EnergyWeight) -- (cidx, # particles, ew/cell)

-- | generate a given number of particles in each cell
genParticlesInCells :: Mesh m => m -> RNG -> [SrcStat] -> 
                       FP ->  -- ^ alpha (power law parameters)
                      [Particle]
genParticlesInCells msh gen nPerCell a = 
  nPerCell >>= (genParticlesInCell msh gen a)

-- | generate a given number of particles in one cell
genParticlesInCell :: Mesh m => m -> RNG ->
                      FP ->  -- ^ alpha (power law parameters) 
                     SrcStat ->
                     [Particle]
genParticlesInCell msh g a (cidx,n,ewt) =
  let gs = take n (unfoldr (Just . split) g)
  in  map (genCellParticle msh cidx ewt a) gs

-- | Generate a single random particle.
genCellParticle :: Mesh m => m -> 
                   CellIdx -> 
                   EnergyWeight ->
                   FP ->  -- ^ alpha  (power law parameters)
                   RNG -> 
                   Particle
genCellParticle msh cidx ewt a g =
  fst $ runRnd g $ do
    let cll = cell msh cidx
        -- Mean energy of Planck-ish Fermionic distribution is 
        -- 7 pi^4/(180 zeta(3)) * (k_B T), ~ 3.15137
        e'  = 3.15137 * (temp . tempE $ mat cll)
    -- to do: sample energy, then sample direction 
    -- using Collision.sampleDirectionIso
    oc <- sampleDirectionIso msh 
    x  <- samplePositionInCell msh cll
    de <- samplePowerLaw a e'  
    let 
        tm  :: Time
        tm  = Time 1e4
        ec  = Energy de
        (en,o) = comovingToLab ec oc (mvel $ mat cll)
    g'   <- get
    -- LT from comoving frame to lab frame
    return (Particle x o tm en ewt cidx g')

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

-- | Compute number of particles emitted in each cell from energy emitted
-- in each cell and total number of particles
srcNumsFromEvol :: Int -> [Energy] -> [(Int,EnergyWeight)]
srcNumsFromEvol nTot ePerCell = zip nPerCell wts
  where nPerCell = [round $ nt * frac  | frac <- fracs] :: [Int]
        fracs    = [ eC/eTot | (Energy eC) <- ePerCell]
        wts      = map EnergyWeight fracs
        Energy eTot = sum ePerCell
        nt :: Double
        nt = fromIntegral nTot 

calcSrcStats :: [Luminosity] -> Time -> Int -> [SrcStat]
calcSrcStats ls dt ntot = zipWith zfunc stats [CellIdx i | i <- [1..n]]
  where stats = srcNumsFromEvol ntot $ evolFromLum ls dt 
        n     = length ls
        zfunc :: (Int,EnergyWeight) -> CellIdx -> SrcStat
        zfunc (i,wt) cidx = (cidx,i,wt)

-- * older generation routine, useful for testing

-- | Generate a number of random particles.
genParticles :: Mesh m => Int -> m -> RNG -> [Particle]
genParticles n msh g =
  let gs = take n (unfoldr (Just . split) g)
  in  map (genParticle msh) gs

-- | Generate a single random particle.
genParticle :: Mesh m => m -> RNG -> Particle
genParticle msh g =
  fst $ runRnd g $ do
    (x,cidx) <- samplePosition  msh
    d        <- sampleDirectionIso msh
    let tm :: Time
        tm = 1
        en :: Energy
        en = 1
        wt :: EnergyWeight
        wt = 1
    g'   <- get
    return (Particle x d tm en wt cidx g')

-- 

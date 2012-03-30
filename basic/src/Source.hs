module Source where

import qualified Data.Vector as U
import Data.Word (Word32)

import Mesh
import Particle
import Physical
import Philo2
import Geometry
import Material
import qualified Data.List as L

type VecLum    = U.Vector Luminosity
type VecEnergy = U.Vector Energy

-- * particle generation

-- | generate a given number of particles in each cell
genParticlesInCells :: Mesh m =>
                       [SrcStat]  
                       -> m          
                       -> FP          -- ^ alpha (power law parameter)
                       -> [RNG]      
                       -> [Particle]
genParticlesInCells ss msh a gens =
  concat $ L.unfoldr (genParticlesInCell msh a) (gens,ss)

-- | generate a given number of particles in one cell
genParticlesInCell :: Mesh m =>
                      m       
                      -> FP       -- ^ alpha (power law parameter) 
                      -> ([RNG],[SrcStat])
                      -> Maybe ([Particle],([RNG],[SrcStat]))
genParticlesInCell _ _ (_,[]) = Nothing
genParticlesInCell msh a (gens,((cidx,n,ewt,_):ss))  = Just (ps,(retGens,ss))
  where
    ps = take ni $ L.unfoldr generator myGs
    (myGs,retGens) = splitAt ni gens
    generator = genCellParticle msh cidx ewt a
    ni = fromIntegral n

-- | generate a single random particle.
genCellParticle :: Mesh m => m 
                   -> CellIdx
                   -> EnergyWeight
                   -> FP            -- ^ alpha  (power law parameter)
                   -> [RNG] -> Maybe (Particle,[RNG])
genCellParticle msh cidx ewt a (g0:gs) =
  let cll = cell msh cidx
      -- Mean energy of Planck-ish Fermionic distribution is 
      -- 7 pi^4/(180 zeta(3)) * (k_B T), ~ 3.15137
      e'  = 3.15137 * (temp . tempE $ mat cll)
  -- to do: sample energy, then sample direction 
  -- using Collision.sampleDirectionIso
      (sel_psn,sel_dir) = random2 g0
      g1 = incrRNG g0
      x  = samplePositionInCell msh cll sel_psn
      oc = sampleDirectionIso msh sel_dir
      (de,_) = samplePowerLaw a e' g1
      tm  :: Time
      tm  = Time 1e12
      ec  = Energy de
      -- Lorentz transform from comoving frame to lab frame
      v      = mvel $ mat cll
      (en,o) = comovingToLab ec oc v
    in Just (Particle x o tm en ewt cidx, gs)


-- * energy sampling

-- | sample energy from power-law approximation to LTE Fermion
-- distribution, as per Keil et al. Ap J 2003. The power law is
--   f_a(e) = (e/e')^a * Exp[-(a+1)*e/e']
-- where a is a parameter dictated by neutrino degeneracy and 
-- e' is the mean energy.
-- We use Aimee Hungerford's rejection method modified to sample a
-- straight exponential and reject using g(y) = y^a Exp(-ay)/Exp(-a).
samplePowerLaw :: FP -> FP -> RNG -> (FP,RNG)
samplePowerLaw a e' gen =
  let (URD y, URD selector) = random2 gen
      x = - log y
      newgen = incrRNG gen
  in if selector <= rejector x a 
     then (x * e', newgen)
     else samplePowerLaw a e' newgen

-- | power law rejection function 
rejector :: FP -> FP -> FP
rejector x a = x**a * (exp (-a*x)) / exp (-a)


-- * source statistics

-- | (cidx, # particles, ew, total energy) for each cell
type SrcStat = (CellIdx,Word32,EnergyWeight,Energy) 

-- | Compute energy emitted in cell volume from luminosity in each cell
evolFromLum :: VecLum -> Time -> [Energy]
evolFromLum vls (Time dt) = 
  let ls = U.toList vls
  in [Energy $ dt * l |(Luminosity l)<-ls]

-- | Compute number of particles emitted in each cell from energy emitted
-- in each cell and total number of particles
srcNumsFromEvol :: Word32 -> [Energy] -> [(Word32,EnergyWeight)]
srcNumsFromEvol nTot ePerCell = zip nPerCell wts
  where nPerCell = [round $ nt * frac  | frac <- fracs] :: [Word32]
        fracs    = [ eC/eTot | (Energy eC) <- ePerCell]
        wts      = [EnergyWeight $ op eC nv | (Energy eC,nv) <- zip ePerCell nPerCell]
        op en n = if n /= 0 then (en/ fromIntegral n) else 0.0
        Energy eTot = sum ePerCell
        nt :: Double
        nt = fromIntegral nTot 

calcSrcStats :: VecLum -> Time -> Word32 -> [SrcStat]
calcSrcStats ls dt ntot = zipWith3 zfunc es stats [CellIdx i | i <- [0..n-1]]
  where stats = srcNumsFromEvol ntot es
        es    = evolFromLum ls dt 
        n     = length es
        zfunc :: Energy -> (Word32,EnergyWeight) -> CellIdx -> SrcStat
        zfunc en (i,wt) cidx = (cidx,i,wt,en)


-- | total number particles in a list of source statistics
nParticles :: [SrcStat] -> Word32
nParticles = L.foldl' (\i (_,j,_,_) -> i + j) 0

-- end of file

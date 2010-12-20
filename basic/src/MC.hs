-- SimpleMC.hs
-- T. M. Kelley
-- Dec 06, 2010
-- Copyright (c) 2010 LANSLLC all rights reserved.

{- Second crack at it--this time with two cells and an opacity.
 - We have five events: scatter, absorb, reflect, transmit, and escape. 
 - Absorption or escape terminates a particle's flight. 
 - 
 - Caller supplies a probability of scattering 
 - called 'sigma'--not really using a cross section yet.    
 -
 - Uses getStdRandom, which commits one to doing a lot in IO. Not  
 - sure whether that's a problem.                                  -}

-- module MC where

import DataTypes
import Constants (c,tiny,huge)
import System.Random (getStdRandom,randomR)
import Data.Map
import Control.Applicative

-- To do: run multiple tallies, generate a per-cell tally


-- push a particle, tally what happens
runParticle :: Particle -> (IO FP) -> Mesh -> Material -> IO EventCount
runParticle p rng mesh mat = tally <$> push p rng mesh mat

{- Use a particle, 'opacity', and an RNG to
 - generate a list of Event, Particle pairs.    -}
push :: Particle -> IO FP -> Mesh -> Material -> IO [(Event,Particle)]
push p rng mesh mat = do 
  sel_s <- rng 
  sel_a <- rng
  sel_o <- rng
  let omega' = isotropicDirection sel_o
      evt = pickEvent p sel_s sel_a omega' mat mesh
      p' = stream p evt omega'
  case evt of 
    -- continuing events
    evt@(Scatter {}) -> 
        stepMsg "Scatter" p' >> ((evt, p'):) <$> (push p' rng mesh mat)
    evt@(Reflect {}) ->  
        stepMsg "Reflect" p' >> ((evt, p'):) <$> (push p' rng mesh mat)
    evt@(Transmit {}) -> 
        stepMsg "Transmit" p' >> ((evt, p'):) <$> (push p' rng mesh mat)
    -- terminal events
    evt@(Escape {})  -> stepMsg "Escape" p' >> return [(evt, p')]
    evt@(Absorb {})  -> stepMsg "Absorb" p' >> return [(evt, p')]
    evt@(Census {})  -> stepMsg "Census" p' >> return [(evt, p')]
  where 
    stepMsg :: String -> Particle -> IO ()
    stepMsg s pp = putStrLn $ s ++ " at x = " ++ show (px pp) ++ ", cell = " 
                      ++ show (pcell pp) ++ ", omega' = " ++ show (pomega p)

pickEvent :: Particle -> 
             FP ->    -- distance to scatter selector (RN)
             FP ->    -- distance to absorption selector (RN)
             FP ->    -- omega': sampled new direction from scatter
                      -- in the co-moving frame 
             Material -> 
             Mesh -> 
             Event
pickEvent p sel_s sel_a omega' mat mesh = 
    let d_scat = -log( sel_s)/sig_s    -- FIX ME: divide by zero opportunity
        d_abs  = -log( sel_a)/sig_a    -- FIX ME: div by zero
        d_bdy  = if omega > 0.0 then (xh - x)/omega else (xl-x)/omega --FIX div 
        d_cen  = c * t where t = pt p
        cellt  = case bdyType of
                 Vacuum      -> Escape 
                 Reflective  -> Reflect
                 Transparent -> Transmit
    in closestEvent [(Scatter d_scat $ dp omega'),  -- record the sampled omega
                     (Absorb  d_abs $ dp 0),
                     (cellt   d_bdy 0),
                     (Census  d_cen 0)]
        where sig_s = maybe tiny sigma_s (Data.Map.lookup cell mat)
              sig_a = maybe tiny sigma_a (Data.Map.lookup cell mat)
              xh    = maybe huge high_x  (Data.Map.lookup cell mesh)
              xl    = maybe huge low_x   (Data.Map.lookup cell mesh)
              bdyType = if omega > 0.0 
                        then maybe Vacuum high_bc (Data.Map.lookup cell mesh)
                        else maybe Vacuum low_bc  (Data.Map.lookup cell mesh)
              x     = px p
              cell  = pcell p
              -- TO DO: boost to co-moving frame, compute the scatter,
              -- boost back to lab frame.
              omega = pomega p
              dp o' = elasticScatterMomDep (penergy p) omega o'

-- sample isotropic distribution
isotropicDirection :: FP -> FP
isotropicDirection rn = 2.0 * rn - 1.0

-- compute the elastic scattering momentum deposition: E/c (k_i^ - k_f)
-- somewhat bogus--won't conserve momentum, also must acct for energy weight
elasticScatterMomDep :: Energy -> FP -> FP -> Momentum
elasticScatterMomDep nrg omega_i omega_f = nrg * (omega_i - omega_f)/c

-- must have at least one event!
closestEvent :: [Event] -> Event
closestEvent evts = foldr compLess (head evts) (tail evts)
    where compLess e1 e2 = if (dist e1) < (dist e2) then e1 else e2


-- actually move particles this time. momentum transfer is dodgy
stream :: Particle -> Event -> FP -> Particle
stream p event omega' = 
    case event of
      Scatter {}  -> p' {pomega=omega'}
      Absorb {}   -> p' {pomega=omega}
      Reflect {}  -> p' {pomega=(-omega)}
      Transmit {} -> p' {pcell=newcell}
      Escape {}   -> p' {pcell=0}
      Census {}   -> p'
    where p'        = p{px = x', pt = t'}
          d         = dist event
          x'        = px p + omega * d
          t'        = pt p - d/c
          omega     = pomega p
          newcell   = if omega > 0.0 then 1 + pcell p else pcell p - 1


-- better than before, still hard to maintain--must hand-coordinate with Event
tallyMom :: [(Event,Particle)] -> MomentumTally 
tallyMom walk = foldr tMom (fromList [(1,0.0),(2,0.0)]) walk
    where tMom :: (Event,Particle) -> MomentumTally -> MomentumTally
          tMom (Scatter _ dp,p) t = adjust (+dp) cell t
              where cell = pcell p
          tMom (Absorb _ dp,p) t = adjust (+dp) cell t
              where cell = pcell p
          tMom _ t    = t

tally :: [(Event,Particle)] -> EventCount
tally walk = foldr countEvent (EventCount 0 0 0 0 0 0) walk

countEvent :: (Event,Particle) -> EventCount -> EventCount
countEvent (Scatter {}, _)  ctr = ctr { n_scatter  = 1 + n_scatter  ctr}
countEvent (Absorb {}, _)   ctr = ctr { n_absorb   = 1 + n_absorb   ctr}
countEvent (Transmit {}, _) ctr = ctr { n_transmit = 1 + n_transmit ctr}
countEvent (Escape {}, _)   ctr = ctr { n_escape   = 1 + n_escape   ctr}
countEvent (Reflect {}, _)  ctr = ctr { n_reflect  = 1 + n_reflect  ctr}
countEvent (Census {}, _)   ctr = ctr { n_census   = 1 + n_census   ctr}



-- instantiations, handy for testing. 
simpleMesh :: Mesh
simpleMesh = fromList [(1::CellIdx,CellProps 0.0 1.0 Vacuum Transparent),
                       (2,         CellProps 1.0 2.0 Transparent Vacuum)]

-- try tuning the scattering and absorption opacities in each cell!
--                                           scat  abs
simpleMat :: Material
simpleMat  = fromList [(1::CellIdx, MatState 2.0   0.2 0.0),
                       (2,          MatState 1.0   0.5 0.0)]

-- for testing, we'll need greater control of the RNG
rand :: (IO FP)
rand = getStdRandom (randomR (0.0 :: FP,1.0))

main :: IO ()
main = do
  let x     = 0.5
  let w     = 1.0
  let t     = 2.5
  let nrg   = 1.0
  let wt    = 1.0
  let cell  = 1
  let p = Particle x w t nrg wt cell
  ecounts <- runParticle p rand simpleMesh simpleMat
  print ecounts
  
-- version
-- $Id$

-- End of file

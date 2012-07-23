-- RunParticles.hs: Different approaches to running particles
-- Feb. 20, 2012
-- (c) Copyright 2012 LANSLLC, all rights reserved

{-# LANGUAGE BangPatterns #-}

module RunParticles where

import Control.Parallel.Strategies
import qualified Control.Monad.Par as Par
import qualified Data.List as L
import Control.DeepSeq (deepseq,rnf)
import Data.Monoid (mappend, mempty)
import Data.Word

import Control.Exception.Base (assert)

import Numerical
import MC
import Mesh
import Source
import Philo2
import Sigma_HBFC
import Tally
import Partition
import Utils (chunkBy,cromulent)


type Key = Philo4x32Key


-- -- | run particles via the Par monad
-- runParticlesPar :: Mesh m =>
--                    Word32   ->   -- ^ chunkSize
--                    m        ->
--                    FP       ->   -- ^ alpha
--                    Lepton   ->
--                    [Key]    ->
--                    SrcStats ->
--                    SrcStats ->
--                   (Word32,Word32) -> 
--                    Tally
-- runParticlesPar chnkSz msh alph lep keys glblStats statsIn (r,c) = 
--   let tllysNu = Par.runPar (runParticlesParW chnkSz msh alph lep keys glblStats statsIn (r,c))
--   in L.foldl' mappend mempty tllysNu
-- {-# INLINE runParticlesPar #-}

-- runParticlesParW :: Mesh m =>
--                     Word32 ->   -- ^ chunkSize
--                     m ->
--                     FP ->    -- ^ alpha
--                     Lepton ->
--                     [Key] ->
--                     SrcStats ->
--                     SrcStats ->
--                     (Word32,Word32) ->
--                     Par.Par [Tally]
-- runParticlesParW chnkSz msh alph lep keys globalStats statsIn (r,c) = do
--   let statz :: [SrcStats]
--       -- Try reversing the list so we don't belatedly discover a hefty
--       -- chunk of work in the outermost streaming region of the star.
--       statz = reverse $! L.unfoldr (takeNParticlesM chnkSz) statsIn

--   Par.parMap (statsToTally chnkSz msh lep alph (r,c) globalStats) (zip keys statz)
-- {-# INLINE runParticlesParW #-}

statsToTally ::  Mesh m =>
                 m        ->
                 Lepton   ->
                 FP       ->    -- ^ alpha
                 Key      -> 
                 (SrcStats,[Word32]) ->
                 Tally
statsToTally msh lep alpha key (!stats, ptclIds) =
  let
    particles :: [Particle]
    particles = -- trace ("chunk " ++ show key ++ ": len ptclIds: " ++ 
                --        show (length ptclIds) ++ "; nstats: " ++ 
                --        show (nParticles stats)) $
                genParticlesInCells stats msh alpha ptclGens
    evts :: [(Event,Particle)]
    evts = stepsPs lep msh particles evtGens
    ptclGens = map (\i -> RNG (incrCtr4 zeroCtr (Offset i)) key) ptclIds
    evtGens  = map (\i -> RNG (incrCtr2 zeroCtr (Offset i)) key) ptclIds
  in tally msh evts
{-# INLINE statsToTally #-}  

-- | Run particles using parallel strategy parList
runParticlesParList :: Mesh m =>
                     Word32   ->    -- ^ chunkSize
                     m        ->
                     FP       ->    -- ^ alpha
                     (Word32,Word32) ->   -- ^ rank (for dist. parallel)
                     Lepton   ->
                     Key      ->
                     SrcStats ->
                     SrcStats ->
                     Tally
runParticlesParList chnkSz msh alph (r,c) lep key glblStats statsIn =
  let stats :: [SrcStats]
      statsf =  L.unfoldr (takeNParticlesM chnkSz) statsIn
      stats  = reverse $ statsf
      ptclIdsInit = genPtclIds r c glblStats
      ptclIdsf = -- trace ("statsIn: " ++ show statsIn ++ 
                 --        "stats: " ++ show stats ++ 
                 --        "\nglobal stats: " ++ show glblStats ++ 
                 --        "\nmesh: " ++
                 --        show (cells msh) ++ "\nptclIdsInit: " ++ 
                 --        show ptclIdsInit) $ 
                    chunkBy (map nParticles statsf) ptclIdsInit
      ptclIds :: [[Word32]]
      ptclIds = reverse ptclIdsf
      -- TO DO: check that the lengths of each ptclIds sublist corresponds
      -- to the number of particles in each stats sublist.
      tallies = assert (cromulent ptclIds stats)
                stats `deepseq` 
                map (statsToTally msh lep alph key) (zip stats ptclIds)
      res = runEval $ parList rdeepseq tallies
  in L.foldl' mappend mempty res
{-# INLINE runParticlesParList #-}

-- -- | Run particles using the parBuffer strategy
-- runParticlesParBuffer :: Mesh m =>
--                     Word32   ->  -- ^ chunkSize
--                     m        ->
--                     FP       ->  -- ^ alpha
--                     (Word32,Word32)   ->   -- ^ (rank,commSz) (for dist. parallel)
--                     Lepton   ->
--                     [Key]    ->
--                     SrcStats ->
--                     Tally
-- runParticlesParBuffer chnkSz msh alph (rank,commSz) lep keys statsIn =
--   let
--     stats :: [SrcStats]
--     stats = reverse $ L.unfoldr (takeNParticlesM chnkSz) statsIn
--     particleIdz :: [[Word32]]
--     particleIdz = map (genPtclIds rank commSz) stats
--     particlez :: [[Particle]]
--     particlez = zipWith (\ss g -> genParticlesInCells ss msh alph g) stats ptclGens
--     evts :: [[(Event,Particle)]]
--     evts = zipWith (stepsPs lep msh) particlez evtGens
--     tallies :: [Tally]
--     tallies = map (tally msh) evts
--     res = tallies `using` parBuffer 100 rdeepseq
--     ptclGens = map (RNG (incrCtr4 zeroCtr 1)) keys
--     evtGens = zipWith (\k ptclIds -> 
--                          map (\i -> RNG (incrCtr2 zeroCtr i) k) ptclIds)  
--               keys particleIdz
-- --    evtGens  = zipWith (\nps k -> map (\i -> RNG (incrCtr2 zeroCtr (rank+i)) k) [1..nps]) npz keys
--     -- evtGens  = map (RNG zeroCtr) keys
--   in
--     L.foldl' mappend mempty res
-- {-# INLINE runParticlesParBuffer #-}


-- -- | Compute the offsets for RNG counters. In our scheme, every MPI
-- -- rank runs the same number of particles in the same set of cells.
-- -- So the offset for any cell is the number of particles in that cell times
-- -- the rank. When particles are split between chunks, that's cool--they'll
-- -- get hit with a different key. Achieve this by laying out n_c copies of
-- -- n_c * rank, where n_c is the number of particles in a cell. So the length
-- -- of the output is the number of particles in the SrcStats.
-- rngOffsets :: Word32 -> SrcStats -> [Word32]
-- rngOffsets rank ss = L.concatMap cellOffs ncs
--   let cellOffs nc = zipWith (+) (cellBases nc) (offPerCell nc)
--       cellBases nc = replicate nc (nc * rank)
--       offsPerCell nc = [0..(nc-1)]
--       ncs = scanl statSum 0 ss
--       statSum i (_,j,_,_) = i + j

-- L.concatMap (\(_,nc,_,_) -> replicate nc (nc*rank))
-- {--# INLINE rngOffsets #-}


-- chunk the SrcStats list
type SrcStats = [SrcStat]

-- | split a list of source statistics into many pieces
takeNParticles :: Word32 -> ([SrcStat],[SrcStat]) -> ([SrcStat],[SrcStat])
takeNParticles n (hs,[]) = (L.reverse hs,[])
takeNParticles n (hs,(c,nc,ew,e):ts) = 
  if n < nc
  then ( (  ((c,n,ew,e):hs)), (c,nc - n,ew,e):ts)
  else takeNParticles (n - nc) $ ( (c,nc,ew,e):hs, ts)
{-# INLINE takeNParticles #-}

-- unfoldable form
takeNParticlesM :: Word32 -> SrcStats -> Maybe (SrcStats,SrcStats)
takeNParticlesM n [] = Nothing
takeNParticlesM n ss = Just $ takeNParticles n ([],ss)
{-# INLINE takeNParticlesM #-}



-- End of file

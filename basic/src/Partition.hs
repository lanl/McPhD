-- Partition.hs
-- T. M. Kelley
-- Mar 22, 2012
-- (c) Copyright 2012 LANSLLC, all rights reserved

-- | Tools for partitioning particles across processes (ranks, 
-- in MPI parlance)

module Partition 
  ( genPtclIds
  , idsPerCell
  , psPerRank
  , renumberStats )   
  where

import qualified Data.List as L
import Data.Word (Word32)
import Source (SrcStat)
import Control.Parallel.MPI.Simple (Rank, fromRank)

{- Parallel decomposition: We divide particles up within each cell.
 - Every rank r gets nc / commSz particles, plus one extra if 
 -
 -     r < nc `mod` commSz.
 -
 - This has the flaw of potentially loading a lot more work onto 
 - ranks with lower rank number. The alternative is to sacrifice
 - reproducibility (simply replicate) or move toward a 
 - domain-decomposed approach. The latter involves other potential
 - load imbalances.
 -
 - To get the same result regardless of the number of MPI ranks, we
 - associate a counter with every particle. The mapping from rank and
 - communicator size to particle ids is performed by genPtclIds.
 -}

-- | Assign particles from global statistics list
-- (matches how we'll generate RNGs).
renumberStats :: Rank -> Word32 -> [SrcStat] -> [SrcStat]
renumberStats r c ss = map (\(x,n,y,z) -> (x,n2 n,y,z)) ss
  where n2 n = psPerRank (fromRank r) c n

-- | Given rank, total number of ranks, and global statistics,
-- compute a of unique id's for particles. This set should be the same
-- regardless of the number of ranks.
genPtclIds :: Word32 -> Word32 -> [SrcStat] -> [Word32]
genPtclIds r commSz ss  = L.concat $ zipWith (idsPerCell r commSz) cumNs ss
  where
    cumNs = init $ scanl statSum 0 ss
    statSum i (_,j,_,_) = i + j

-- | Generate a list of particle ids for one cell, implementing 
-- id of kth particle in cell j for rank r = 
-- cumN           (cumulative number of all particles in cells c < j
-- + rank offset  (number of particles in ranks p < r)
-- + k            (for all k in [0,nc) ).
idsPerCell :: Word32 -- ^ rank
              -> Word32 -- ^ communicator size
              -> Word32 -- ^ cumulative # particles
              -> SrcStat -> [Word32]
idsPerCell _ _      _    (_,0,_,_)  = []
idsPerCell r commSz cumN (_,nc,_,_) = 
  if ppr > 0
  then [i + cumN + (rankOff nc r commSz) | i <- [0..(ppr-1)] ]
  else []
    where ppr = psPerRank r commSz nc

-- | Offset for rank
rankOff :: Word32 -- ^ rank
           -> Word32 -- ^ communicator size
           -> Word32 -- ^ total number of particles in cell
           -> Word32 
rankOff _ 0 _ = 0
rankOff nc r commSz = sum $ map (\rk -> psPerRank rk commSz nc) [0..(r-1)]

-- | Number of particles per cell per rank, based on comm size
psPerRank :: Word32 -- ^ rank 
             -> Word32 -- ^ communicator size
             -> Word32 -- ^ total number of particles per cell
             -> Word32
psPerRank r commSz nc = 
  let nbase = nc `div` commSz 
      nextra = if r < (nc `mod` commSz) then 1 else 0
  in nbase + nextra

{-# INLINE idsPerCell #-}
{-# INLINE rankOff    #-}
{-# INLINE psPerRank  #-}


-- End of file

-- PRNG.hs
-- T. M. Kelley
-- Mar 03, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved
{-# LANGUAGE BangPatterns #-}
                                                                                
module PRNG (RNG
            ,mkRNG
            ,random
            ,getFiveRNs
            ,getSixRNs
            ,prand)
  where

import System.Random(mkStdGen,StdGen,random,Random)
import Numerical

type RNG = StdGen

mkRNG :: Int -> RNG
mkRNG = mkStdGen

getFiveRNs :: RNG -> (FP,FP,FP,FP,FP,RNG)
getFiveRNs g = let (!r5,!g5) = random g4
                   (!r4,!g4) = random g3
                   (!r3,!g3) = random g2
                   (!r2,!g2) = random g1
                   (!r1,!g1) = random g
  in (r1,r2,r3,r4,r5,g5)

getSixRNs :: RNG -> (FP,FP,FP,FP,FP,FP,RNG)
getSixRNs g = let  (!r6,!g6) = random g5
                   (!r5,!g5) = random g4
                   (!r4,!g4) = random g3
                   (!r3,!g3) = random g2
                   (!r2,!g2) = random g1
                   (!r1,!g1) = random g
  in (r1,r2,r3,r4,r5,r6,g6)

prand :: RNG
prand =  mkStdGen 42



-- version
-- $Id$

-- End of file

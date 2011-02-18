{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- allow newtype to derive Num
-- Numerical.hs
-- T. M. Kelley
-- Jan 28, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Numerical (FP
                 , Idx
                 , VecT
                 , CellIdx(..)
                 , Tag
                 , xcomp, ycomp, zcomp
                 , RandSeed
                 , RNG(..)
                 , rand
                 , module Data.Word
                 , module Data.Vector.V1
                 , module Data.Vector.Class
                 , module Data.Array)
    where

import Data.Vector.Class (vmag,(*|))
import Data.Word 
import Data.Array (Ix,(!),listArray,Array)
import System.Random (getStdRandom,randomR)

-- should have some way of more easily (and consistently) switching between 1D 
-- and 3D. 
import Data.Vector.V1

type VecT = Vector1
xcomp,ycomp,zcomp :: VecT -> FP
xcomp = v1x
ycomp _ = 0.0 :: FP
zcomp _ = 0.0 :: FP
{-
 -- for 3D, something like
import Data.Vector.V3
type VecT = Vector3
xcomp = v3x
ycomp = v3y
zcomp = v3z
-}

type FP   = Double
type Idx  = Word32
type Tag  = Word32 -- used for tagging things uniquely within their type

type RandSeed = FP

newtype CellIdx = CellIdx {idx :: Idx } deriving (Eq,Show,Num,Ord,Ix,Integral,Real,Enum)

newtype RNG = RNG { random :: (IO FP)} 

-- for testing, we'll need greater control of the RNG
rand :: (IO FP)
rand = getStdRandom (randomR (0.0 :: FP,1.0))


-- version
-- $Id$

-- End of file

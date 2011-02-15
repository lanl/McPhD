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
                 , module Data.Word
                 , module Data.Vector.V1
                 , module Data.Vector.Class
                 , module Data.Array)
    where

import Data.Vector.Class (vmag,(*|))

import Data.Word 
import Data.Array (Ix,(!),listArray,Array)

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

newtype CellIdx = CellIdx {idx :: Idx } deriving (Eq,Show,Num,Ord,Ix)

newtype RNG = RNG { random :: (IO FP)} 


-- version
-- $Id$

-- End of file

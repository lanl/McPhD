{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Physical.hs
-- T. M. Kelley
-- Jan 28, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

-- Physical types

module Physical (Position(..)
                , Direction(..)
                , Momentum(..)
                , Velocity(..)
                , Energy (..)
                , Time(..)
                , EnergyWeight(..)
                , Opacity(..)
                , Temperature(..)
                , dOnRay
                , module Numerical
                , module Constants)
    where
      
import Numerical
import Constants

newtype Position     = Position     { pos :: VecT } deriving (Eq, Show,Num)
newtype Direction    = Direction    { dir :: VecT } deriving (Eq, Show,Num)
newtype Momentum     = Momentum     { mom :: VecT } deriving (Eq, Show,Num)
newtype Velocity     = Velocity     { vel :: VecT } deriving (Eq, Show,Num)
newtype Energy       = Energy       { e :: FP }     deriving (Eq,Show,Num)
newtype Time         = Time         { t :: FP }     deriving (Eq, Show,Num)
newtype EnergyWeight = EnergyWeight {ew :: FP }     deriving (Eq, Show,Num)

-- | opacity, derived (perhaps) from density * cross-section [cm^-1]
newtype Opacity = Opacity { sigma :: FP }           deriving (Eq,Show,Num)
newtype Temperature = Temperature { temp :: FP }    deriving (Eq,Show,Num)

-- | distance 
dOnRay ::  (VecT -> FP) -> Position -> Direction -> FP
dOnRay comp p o | ocomp /= 0.0 = pcomp/ocomp
                | otherwise = huge
                where ocomp = comp.dir $ o; pcomp = comp.pos $ p

-- version
-- $Id$

-- End of file

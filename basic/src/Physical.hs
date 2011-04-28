{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Physical (
    module Physical
  , module Constants
  , module Numerical
  )
  where

import Data.Vector.Generic.Base as GV
import Data.Vector.Generic.Mutable as GMV
import Data.Vector.Unboxed as V

import Constants
import Numerical

newtype Position     = Position     { pos   :: Vec } deriving (Eq, Show)
newtype Direction    = Direction    { dir   :: Vec } deriving (Eq, Show, Num)
newtype Momentum     = Momentum     { mom   :: Vec }
  deriving (Eq, Show, Num, GV.Vector V.Vector, GMV.MVector V.MVector, Unbox)
newtype Velocity     = Velocity     { vel   :: Vec } deriving (Eq, Show)
newtype Energy       = Energy       { e     :: FP  } deriving (Eq, Show, Num)
newtype EnergyWeight = EnergyWeight { ew    :: FP  }
  deriving (Eq, Show, Num, GV.Vector V.Vector, GMV.MVector V.MVector, Unbox)
newtype Time         = Time         { t     :: FP  } deriving (Eq, Show, Num)

newtype Opacity      = Opacity      { sigma :: FP  } deriving (Eq, Show)
newtype Temperature  = Temperature  { temp  :: FP  } deriving (Eq, Show)

-- Note: in moving to 3D, we'll have to be careful about speed vs velocity

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

newtype CrossSection = CrossSection { sigma :: FP  } deriving (Eq, Show, Num)
newtype Opacity      = Opacity      { mu    :: FP  } deriving (Eq, Show, Num)
newtype Temperature  = Temperature  { temp  :: FP  } deriving (Eq, Show)
newtype Density      = Density      { rho   :: FP  } deriving (Eq, Show)
newtype NucleonNumber = NucleonNumber {nnucl :: FP } deriving (Eq, Show, Num)
newtype Distance     = Distance     { distance :: FP } deriving (Eq, Show, Num, Ord)

-- end of file

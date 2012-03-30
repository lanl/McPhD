{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances #-}
module Physical (
    module Physical
  , module Constants
  , module Numerical
  )
  where

import Control.DeepSeq
import Data.Vector.Generic.Base as GV
import Data.Vector.Generic.Mutable as GMV
import Data.Vector.Unboxed as V
import Data.Serialize
import Constants
import Numerical

-- | cm
newtype Position     = Position     { pos   :: Vec }
  deriving (Eq,Show,Read,Num,Fractional,GV.Vector V.Vector,GMV.MVector V.MVector,Unbox)
-- | dimensionless
newtype Direction    = Direction    { dir   :: Vec }
  deriving (Eq,Show,Read,Num)
-- | g cm/sec
newtype Momentum     = Momentum     { mom   :: Vec }
  deriving (Eq,Show,Read,Num,GV.Vector V.Vector,GMV.MVector V.MVector,Unbox,Serialize)
-- | cm/sec
newtype Velocity     = Velocity     { vel   :: Vec }
  deriving (Eq,Show,Read,GV.Vector V.Vector,GMV.MVector V.MVector,Unbox)
-- | MeV
newtype Energy       = Energy       { e     :: FP  }
  deriving (Eq, Show, Read, Ord, Num, Fractional, Floating, GV.Vector V.Vector, GMV.MVector V.MVector, Unbox, NFData,Serialize)
-- | dimensionless
newtype EnergyWeight = EnergyWeight { ew    :: FP  }
  deriving (Eq, Show, Read, Ord, Num, Fractional, Floating, GV.Vector V.Vector, GMV.MVector V.MVector, Unbox, NFData)
-- | sec
newtype Time         = Time         { t     :: FP  }
  deriving (Eq,Show,Read,Num)
-- | cm ^2
newtype CrossSection = CrossSection { sigma :: FP  }
  deriving (Eq,Show,Read,Num)
-- | cm ^ -1
newtype Opacity      = Opacity      { mu    :: FP  }
  deriving (Eq,Show,Read,Num)
-- | MeV
newtype Temperature  = Temperature  { temp  :: FP  }
  deriving (Eq,Show,Read,GV.Vector V.Vector,GMV.MVector V.MVector,Unbox)
-- | g/(cc^3)
newtype Density      = Density      { rho   :: FP  }
  deriving (Eq,Show,Read,GV.Vector V.Vector,GMV.MVector V.MVector,Unbox)
-- | 1/(cc^3)
newtype NDensity     = NDensity     { nrho   :: FP }
  deriving (Eq,Show,Read,GV.Vector V.Vector,GMV.MVector V.MVector,Unbox)
-- | dimensionless
newtype NucleonNumber = NucleonNumber {nnucl :: FP }
  deriving (Eq,Show,Read,Num)
-- | cm
newtype Distance     = Distance     { distance :: FP }
  deriving (Eq, Show, Read, Num, Ord, NFData,Serialize)
-- | MeV/sec
newtype Luminosity   = Luminosity   { lum :: FP }
  deriving (Eq,Show,Read,Num,Ord,GV.Vector V.Vector,GMV.MVector V.MVector,Unbox)
-- | dimensionless
newtype ElectronFraction = ElectronFraction {ef :: FP}
  deriving (Eq,Show,Read,Num,Ord)

-- end of file

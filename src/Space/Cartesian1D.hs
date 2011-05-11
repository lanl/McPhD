{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}

module Space.Cartesian1D where

import Approx
import NormalizedValues
import Space.Classes
import Data.Vector.V2


data Cartesian1D = Cartesian1D { pos :: Double
                               , dir :: Normalized Vector2 }
                   deriving (Eq, Show)

instance Space Cartesian1D where
  type Distance Cartesian1D = Double
  type Position Cartesian1D = Double
  type Direction Cartesian1D = Normalized Vector2

  position  = pos
  direction = dir
  stream loc@(Cartesian1D pos dir) distance
      = loc { pos = pos + distance*dx }
        where dx = v2x $ normalized_value dir

instance Approx Cartesian1D where
  within_eps epsilon (Cartesian1D x1 d1) (Cartesian1D x2 d2) =
    (within_eps epsilon x1 x2) && (within_eps epsilon d1 d2)

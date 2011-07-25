{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}

module Space.Cartesian1D where

import Data.Vector.V2

import Space.Classes
import Approx
import Properties
import NormalizedValues

-- This is a very long comment designed to test haskell-mode
-- auto-wrapping feature in a new buffer


data Cartesian1D = Cartesian1D { pos :: Double
                               , dir :: Normalized Vector2 }
                   deriving (Eq, Show)

instance Space Cartesian1D where
  type Position  Cartesian1D = Double
  type Direction Cartesian1D = Normalized Vector2
  type Velocity  Cartesian1D = Vector2

  position  = pos
  direction = dir
  stream loc@(Cartesian1D pos dir) (Distance d)
      = loc { pos = pos + d*dx }
        where dx = v2x $ getValue dir

instance Approx Cartesian1D where
  within_eps epsilon (Cartesian1D x1 d1) (Cartesian1D x2 d2) =
    (within_eps epsilon x1 x2) && (within_eps epsilon (getValue d1) (getValue d2))

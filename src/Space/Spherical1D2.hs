{-# LANGUAGE TypeSynonymInstances, TypeFamilies #-}

module Space.Spherical1D2 where

import Data.Vector.Class
import Data.Vector.V2

import Space.Classes
import Numerics
import NormalizedValues
import Approx


type Spherical1D2 = Vector2

instance Space Spherical1D2 where
    type Distance  Spherical1D2 = Double
    type Position  Spherical1D2 = Radius
    type Direction Spherical1D2 = Normalized Vector2
    stream (Vector2 x y) dist = Vector2 (x+dist) y
    position s  = Radius $ vmag s
    direction s = normalize s
    


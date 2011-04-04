module Vectors where

import Data.Vector.V2
import Data.Vector.V3
import Data.Vector.Class

polarToVector2 :: Double -> Double -> Vector2
polarToVector2 radius phi = radius *| polarToNormalVector2 phi

polarToNormalVector2 :: Double -> Vector2
polarToNormalVector2 phi = Vector2 (cos phi) (sin phi)

sphericalToVector3 :: Double -> Double -> Double -> Vector3
sphericalToVector3 radius phi theta = radius *| (sphericalToNormalVector3 phi theta)

sphericalToNormalVector3 :: Double -> Double -> Vector3
sphericalToNormalVector3 phi theta = let
  x = (sin theta) * (cos phi)
  y = (sin theta) * (sin phi)
  z = (cos theta)
  in Vector3 x y z

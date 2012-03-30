module Vectors where

import Numerics
import Data.Vector.V2
import Data.Vector.V3
import Data.Vector.Class

polarToCartesian :: Radius -> AzimuthAngle -> Vector2
polarToCartesian (Radius radius) phi = radius *| polarToNormalCartesian phi

polarToNormalCartesian :: AzimuthAngle -> Vector2
polarToNormalCartesian (AzimuthAngle phi) = Vector2 (cos phi) (sin phi)

sphericalToCartesian :: Radius -> AzimuthAngle -> ZenithAngle -> Vector3
sphericalToCartesian (Radius radius) phi theta = radius *| (sphericalToNormalCartesian phi theta)

sphericalToNormalCartesian :: AzimuthAngle -> ZenithAngle -> Vector3
sphericalToNormalCartesian (AzimuthAngle phi) (ZenithAngle theta) = let
  x = (sin theta) * (cos phi)
  y = (sin theta) * (sin phi)
  z = (cos theta)
  in Vector3 x y z



{-# LANGUAGE TypeSynonymInstances, TypeFamilies #-}

module Space.Spherical1D where
{-| Spherical 1D space is for problems with the rotational symmetry of
the sphere. The only location information we need in this space is the
distance from the origin, and a direction of motion measured from the
radial outward vector.
                                    _
                                    /| dir_vec = <\xi, \eta>
                                   /
                                  /
                                 /
                  r             /  phi
      O------------------------P-----------> r_vec
   Origin                   Particle

The equations of motion in this coordinate system are most naturally
expressed in terms of r^2, r\xi and r\eta:

  r\xi  <- r\xi + d
  r\eta <- r\eta
  r^2   <- r^2 + 2dr\xi + d^2

where dir_vec = <\xi, \eta>.

Because r^2 = (r\xi)^2 + (r\eta)^2, we drop it as seperate quantity,
and just store <r\xi, r\eta> as a regular Vector2.

--}

import Data.Vector.Class
import Data.Vector.V2

import Space.Classes
import Numerics
import Properties
import NormalizedValues


type Spherical1D = Vector2

instance Space Spherical1D where
    type Position  Spherical1D = Radius
    type Direction Spherical1D = Normalized Vector2
    type Velocity  Spherical1D = Vector2
    stream (Vector2 x y) (Distance d) = Vector2 (x+d) y
    position s  = Radius $ vmag s
    direction s = normalize s
    scale direction scalar = (normalized_value direction) |* scalar
    make (Radius pos) dir = pos *| (normalized_value dir)


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

We can also assume, without loss of generality, that \eta >= 0, since
other eta values can be obtained via rotation.

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
    scale _ direction scalar = getValue direction |* scalar
    make (Radius pos) dir = pos *| (fix_eta $ getValue dir)
      where fix_eta (Vector2 x y) = Vector2 x (abs y)


-- Stand-alone functions for these operations. I'm considering trying
-- to provide these as arguments to functions which need them; instead
-- of programming to the Space typeclass. I didn't define the instance
-- in terms of these because they might dissapear at any moment.

sph1Dstream :: Spherical1D -> Distance -> Spherical1D
sph1Dstream (Vector2 x y) (Distance d) = Vector2 (x+d) y

sph1Dposition :: Spherical1D -> Radius
sph1Dposition = Radius . vmag

sph1Ddirection :: Spherical1D -> Normalized Vector2
sph1Ddirection = normalize

sph1DdirScale :: Normalized Vector2 -> Double -> Vector2
sph1DdirScale v scalar = (getValue v) |* scalar

sph1Dmake :: Radius -> Normalized Vector2 -> Spherical1D
sph1Dmake (Radius r) v = sph1DdirScale v r

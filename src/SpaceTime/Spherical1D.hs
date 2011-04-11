{-# LANGUAGE TypeFamilies #-}
module SpaceTime.Spherical1D where
{-- Spherical 1D space is for problems with the rotational symmetry of
the sphere. The only location information we need in this space is the
distance from the origin, and a direction of motion measured from the
radial outward vector.

                                    _
                                    /| dir_vec
                                   /
                                  /
                                 /
                  r             /  phi
      O------------------------P-----------> r_vec
   Origin                   Particle

Because it is convienent, we store the direction of motion as a
normalized 2-vector, even though only one component is necessary.

--}

import SpaceTime.Classes
import Numerics
import NormalizedValues
import Data.Vector.V2
import Approx

data Spherical1D = Spherical1D { sph1d_position :: Radius, sph1d_direction :: Normalized Vector2 }
                 deriving (Eq, Show)

instance Space Spherical1D where
  type Distance Spherical1D = Double
  type Position Spherical1D = Radius
  type Direction Spherical1D = Normalized Vector2
  stream (Spherical1D (Radius r) direction) dist =
    Spherical1D  (Radius r') direction'
      where cos_phi    = (v2x . normalized_value) direction
            sin_phi    = (v2y . normalized_value) direction
            r'         = sqrt (r*r + dist*dist + 2*r*dist*cos_phi)
            cos_phi'   = (cos_phi*r + dist) / r'
            sin_phi'   = sin_phi*r/r'
            direction' = unsafe_makeNormal $ Vector2 cos_phi' sin_phi'
  position  = sph1d_position
  direction = sph1d_direction

instance Approx Spherical1D where
  within_eps epsilon (Spherical1D r1 d1) (Spherical1D r2 d2) =
    (within_eps epsilon r1 r2) && (within_eps epsilon d1 d2)
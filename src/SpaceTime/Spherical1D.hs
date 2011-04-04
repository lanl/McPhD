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
import Data.Vector.V2

data Spherical1D = Spherical1D { position :: Double, direction :: Vector2 } deriving (Eq, Show)

instance Space Spherical1D where
  type Distance Spherical1D = Double
  stream (Spherical1D r (Vector2 cos_phi sin_phi)) dist = Spherical1D r' (Vector2 cos_phi' sin_phi')
      where r'       = sqrt (r*r + dist*dist + 2*r*dist*cos_phi)
            tan_phi' = r * sin_phi / (dist + r * cos_phi)
            cos_phi' = 1 / sqrt (tan_phi' * tan_phi' + 1)
            sin_phi' = tan_phi' * cos_phi'
    

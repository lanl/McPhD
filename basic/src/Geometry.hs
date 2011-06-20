module Geometry where

import Physical
import Constants (c)

-- | Lorentz factor
gamma :: FP -> FP
gamma s = 1.0/sqrt(1.0 - s*s/(c*c))

-- | Convert energy and direction from co-moving frame to lab frame,
-- where the comoving frame has velocity v IN THE LAB FRAME (!)
comovingToLab :: Energy -> Direction -> Velocity -> (Energy,Direction)
comovingToLab (Energy nrg) (Direction omega) (Velocity v) = (e',omega')
    where e'     = Energy $ nrg * (gamma v)  * (1.0 + omega * v/c)
          omega' = Direction $ (omega + v/c)/(1.0 + omega*v/c)

-- | Convert energy and direction from lab frame to co-moving frame,
-- where the comoving frame has velocity v in the lab frame. The energy 
-- is the relativistic Doppler shift, while the change in direction is 
-- the relativistic aberration.
labToComoving :: Energy -> Direction -> Velocity -> (Energy,Direction)
labToComoving (Energy nrg) (Direction omega) (Velocity v) = (e',omega')
    where e'     = Energy $ nrg * (gamma v) * (1.0 - omega * v/c)
          omega' = Direction $ (omega - v/c)/(1.0 - omega*v/c)

-- end of file

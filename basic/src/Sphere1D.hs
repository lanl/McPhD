-- Sphere1D.hs: 1D spherical mesh functions
-- T. M. Kelley
-- Feb 15, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Sphere1D (sampDir
                ,sampPos
                ,distToBdy)
    where

import SoftEquiv
import Physical
import Cell 
import Data.Array (bounds)
import MeshBase

sampDir :: RNG -> IO Direction
sampDir rng = random rng >>= \x -> return (Direction . Vector1 $ 2*x -1)

-- | sample a position in a sphere
sampPos :: Mesh -> RNG -> IO (Position,CellIdx)
sampPos msh rng = do
  r <- pickPt (rmax msh) rng
  let pos = (Position . Vector1) r
      cell = findCell msh r
  return (pos,cell)

-- | rejection method to select an r-coordinate
pickPt :: FP -> RNG -> IO FP
pickPt r rng = do
  x <- random rng
  y <- random rng
  z <- random rng
  let r' = sqrt x*x + y*y + z*z
  if r' < r
    then return r' 
    else pickPt r rng 


-- b search in the mesh
findCell :: Mesh -> FP -> CellIdx
findCell msh r = impl msh r f l
    where impl msh r lo hi | lo == hi  = lo
                           | val < r   = impl msh r (idx+1) hi
                           | otherwise = impl msh r lo idx
                           where idx = (lo+hi) `quot` 2
                                 val = xcomp . pos . low_b $ mesh msh ! idx
          f = fst . bounds $ mesh msh
          l = snd . bounds $ mesh msh

-- | Distance to boundary in spherical 1D coordinates. 
{- Check for intersection with two concentric spheres with radius rHi and rLow.
 - Choose the first intersection along the line of travel. For the outer sphere
 - there will be one intersection that we care about. For the inner sphere, 
 - there will be 0, 1, or 2 intersections.  In each case, the intersection of 
 - a sphere and a line may be written as (x,y) where
 -     x = (r * t^2)/(1+t^2) +- det,
 -     y = -r*t + r * t^3/(1+t^2) +- t * det, 
 -     t = tan(theta), theta is the polar angle, and 
 -     det = (r_sph * (1+t^2) - r^2 * t^2)^{1/2}/(1+t^2)
 -     r_sph = the radius of the sphere, and r is the current coordinate 
 - We're counting on laziness to bail us out if there is not intersection 
 - with the inner sphere (in that case, the sqrt in the determinant will be 
 - complex valued, or more likely, NaN).
 -}
distToBdy :: FP -> FP -> FP -> FP -> (FP,Face) 
distToBdy r omega rhi rlow = 
    if (contactInner r rlow t thetaLTpiOver2)
    then (dlo, XLow)
    else (dhi, XHigh)
        where 
          dhi = sqrt( (xhi - r) * (xhi-r) + yhi*yhi)
          dlo = sqrt( (xlo - r) * (xlo-r) + ylo*ylo)
          -- These define the two intersections with the outer sphere
          xhip = xterm1 + dethi; yhip = yterm1 + t * dethi 
          xhim = xterm1 - dethi; yhim = yterm1 - t * dethi 
          -- if the polar angle is less than pi/2, take the solution that
          -- adds the determinant, otherwise the one that subtracts
          (xhi,yhi) = if thetaLTpiOver2 then (xhip,yhip) else (xhim,yhim) 
          -- intersection with the inner sphere: just use the (+ det) soln
          (xlo,ylo) = (xterm1 + detlo, yterm1 + t * detlo)
          -- these pieces are independent of the radius of the sphere:
          xterm1 = r * tsq * oneOverOnePTSq
          yterm1 =  -r * t + r * t * tsq * oneOverOnePTSq
          -- determinant: dependence on sphere radius (rs) is here
          det = \rs -> sqrt(rs*rs * onePTSq - r * r * tsq) * oneOverOnePTSq
          dethi = det rhi
          detlo = det rlow
          -- will the ray contact the inner sphere? 
          contactInner :: FP -> FP -> FP -> Bool -> Bool
          contactInner r rlow tanTheta thetaLTpiOver2 = rlow > 0 
                  && not (softEquiv b 1.0 1e-11)  -- not _on_ the inner sphere
                  && (tanTheta <= tanThetaLim && thetaLTpiOver2) -- moving toward inner
                  where b = r/rlow
                        tanThetaLim = sqrt(1/(b*b-1))
          theta = acos(omega)
          thetaLTpiOver2 = theta < pi/2
          t = if thetaLTpiOver2    -- work with the polar angle mapped into [0,pi/2]
              then tan(theta)
              else tan(pi - theta)
          -- abbreviations
          tsq = t * t
          onePTSq = 1 + tsq
          oneOverOnePTSq = 1/onePTSq

-- maximum value of r
rmax :: Mesh -> FP
rmax msh = xcomp . pos.high_b $ arr ! bmax
           where arr = mesh msh
                 bmax = snd (bounds arr)

-- version
-- $Id$

-- End of file

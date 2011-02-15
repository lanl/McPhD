-- Mesh.hs
-- T. M. Kelley
-- Jan 28, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Mesh (Mesh(..)
            , bdyEvent
            , distToBdy
            , simpleMesh
            , module Cell)
    where

import SoftEquiv
import Physical
import Event
import Cell 
-- import Data.Vector.V1
-- import Data.Vector.V3

-- * Connectivity and extents
-- | Mesh properties for each cell
data Mesh = Sphere1D { mesh :: Array CellIdx CellProperties } 
          | Cart3D   { mesh :: Array CellIdx CellProperties }
          | Cart1D   { mesh :: Array CellIdx CellProperties }
            deriving (Show)

-- | Given a mesh,cell, and face, determine the Event ctor
bdyEvent :: Mesh -> CellIdx -> Face -> (FP->Momentum->Face->Event)
bdyEvent msh cell face = bdyTypeToEvent $ bdyType msh cell face

-- | look up type of boundary from mesh 
bdyType :: Mesh -> CellIdx -> Face -> BoundaryCondition
bdyType msh cell XLow  = xbc $ low_bc  ((mesh msh) ! cell)
bdyType msh cell XHigh = xbc $ high_bc ((mesh msh) ! cell)
bdyType msh cell YLow  = ybc $ low_bc  ((mesh msh) ! cell)
bdyType msh cell YHigh = ybc $ high_bc ((mesh msh) ! cell)
bdyType msh cell ZLow  = zbc $ low_bc  ((mesh msh) ! cell)
bdyType msh cell ZHigh = zbc $ high_bc ((mesh msh) ! cell)

-- | Given a boundary condition, return the corresponding event ctor
bdyTypeToEvent :: BoundaryCondition -> (FP->Momentum->Face->Event)
bdyTypeToEvent Vac = Escape
bdyTypeToEvent Refl = Reflect
bdyTypeToEvent Transp = Transmit
-- a 'None' BC represents failure

-- * geometric properties
-- | dispatch on mesh type to appropriate distance function
distToBdy :: Mesh -> CellIdx -> Position -> Direction -> (FP, Face)
distToBdy msh cell psn drn = 
    case msh of 
      Sphere1D {} -> distToBdySphere1D r omega rhi rlo
          where r     = v1x (pos psn)
                omega = v1x (dir drn)
                rhi   = xcomp (pos $ high_b ((mesh msh) ! cell))
                rlo   = xcomp (pos $ low_b  ((mesh msh) ! cell))
      Cart3D {} -> distToBdyCart3D x o bl bh
      Cart1D {} -> distToBdyCart1D x o bl bh
      where x  = psn
            o  = drn
            bl = high_b ((mesh msh) ! cell)
            bh = low_b  ((mesh msh) ! cell)

-- | Distance to boundary in cartesian 1D coordinates
{- Create a list of pairs of distance to high & low faces divided by direction
 - cosine in each dimension and the corresponding face. Filter list to 
 - remove the element with negative distance. This leaves one choice, which
 - we take. This all relies on a well-formed mesh, in which the bounds are 
 - monotonically increasing. -}
distToBdyCart1D :: Position -> Direction -> Position -> Position -> (FP, Face)
distToBdyCart1D psn drn b_low b_high = 
    let dnfs = [((xcomp lows)/(xcomp d),XLow),((xcomp highs)/(xcomp d),XHigh)]
        lows  = pos(psn - b_low)   --  -ve: chosen if omega < 0
        highs = pos(psn - b_high)  --  +ve: chosen if omega > 0
        d = dir drn
    in head (filter (\(a,b) -> a>=0) dnfs)

-- | Distance to boundary in cartesian 3D coordinates
{- Create a list of pairs of distance to high & low faces divided by direction
 - cosine in each dimension and the corresponding face. Filter list to 
 - remove elements with negative distance. Pass to helper function that
 - finds the pair with least distance. As in the 1D case, this relies on
 - a well-formed mesh, in which the bounds are monotonically increasing. 
 - If this function is called with Vector1's, there will be many divide-by-0's,
 - from a correctness perspective, it looks like the program will do the right
 - thing, but that may be a performance killer. -}
distToBdyCart3D :: Position -> Direction -> Position -> Position -> (FP, Face)
distToBdyCart3D psn drn b_low b_high = 
    let dnfs = [((xcomp lows)/(xcomp d),XLow),((xcomp highs)/(xcomp d),XHigh),
                ((ycomp lows)/(ycomp d),YLow),((ycomp highs)/(ycomp d),YHigh),
                ((zcomp lows)/(zcomp d),ZLow),((zcomp highs)/(zcomp d),ZHigh)]
        lows  = pos(psn - b_low)   --  -ve: chosen if omega < 0
        highs = pos(psn - b_high)  --  +ve: chosen if omega > 0
        d = dir drn
    in closestFace (filter (\(a,b) -> a>=0) dnfs)

closestFace :: [(FP,Face)] -> (FP,Face)
closestFace dnfs = foldr compLess (head dnfs) (tail dnfs) -- dnf: distance & face
    where compLess dnf1 dnf2 = if (fst dnf1) < (fst dnf2) then dnf1 else dnf2


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
distToBdySphere1D :: FP -> FP -> FP -> FP -> (FP,Face) 
distToBdySphere1D r omega rhi rlow = 
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

-- instantiation, handy for testing. 
smBounds = (1::CellIdx,2::CellIdx)
simpleMesh :: Mesh
simpleMesh = Sphere1D $ listArray smBounds 
             [(CellProps (Position 0.0) (Position 1.0) (bc1D Vac) (bc1D Transp)),
              (CellProps (Position 1.0) (Position 2.0) (bc1D Transp) (bc1D Vac))]

                                     
-- version
-- $Id$

-- End of file

module Sphere1D where

import Control.Monad
import Data.List as L
import Data.Vector as V

import Cell
import Mesh
import PRNG
import Physical
import Search
import SoftEquiv

data Sphere1D = Sphere1D (Vector Cell) deriving Show

instance Mesh Sphere1D where

  sampleDirectionIso _ = liftM (\ x -> Direction (2 * x - 1)) random

  samplePositionInCell _ c = do 
    xi <- random
    let ri3 = ri * ri * ri
        ro3 = ro * ro * ro
        (ri,ro) = (pos . lowB $ c, pos . highB $ c)
        psn = Position ( (ri3 + (ro3 - ri3) * xi) ** (1.0/3.0) )
    return psn

  samplePosition msh = do
    r <- pickPoint (rmax msh)
    let pos  = Position r
        cell = findCell msh r
    return (pos, cell)

  -- distanceToBoundary :: m -> Cell -> Position -> Direction -> (Distance, Face)
  distanceToBoundary _ (Cell {highB = rhi, lowB = rlo})
                     p@(Position r) (Direction omega)
    | contactInner p rlo t thetaLTpiOver2 = (Distance dlo, Lo)
    | otherwise                           = (Distance dhi, Hi)
    where
      -- (rhi,rlo) = (highB cll, lowB cll)
      -- Cell { highB = rhi, lowB = rlo } = msh ! cidx

      dhi = sqrt ((xhi - r)*(xhi - r) + yhi*yhi)
      dlo = sqrt ((xlo - r)*(xlo - r) + ylo*ylo)

      -- These define the two intersections with the outer sphere
      xhip = xterm1 +     dethi
      yhip = yterm1 + t * dethi
      xhim = xterm1 -     dethi
      yhim = yterm1 - t * dethi

      -- If polar angle is less than pi/2, take the solution that
      -- adds the determinant, otherwise the one that subtracts
      (xhi, yhi) | thetaLTpiOver2 = (xhip, yhip)
                 | otherwise      = (xhim, yhim)

      -- These are independent of the sphere radius
      xterm1 = r * tsq * oneOverOnePTSq
      yterm1 = -r * t + r * t * tsq * oneOverOnePTSq

      -- Intersection with the inner sphere: just use the
      -- (+ det) solution
      xlo = xterm1 +     detlo
      ylo = yterm1 + t * detlo

      -- Determinant; depends on sphere radius rs is here
      det rs = sqrt (rs*rs * onePTSq - r*r * tsq) * oneOverOnePTSq
      dethi  = det (pos rhi)
      detlo  = det (pos rlo)

      theta = acos omega
      thetaLTpiOver2 = theta < pi / 2

      t | thetaLTpiOver2 = tan theta
        | otherwise      = tan (pi - theta)
      tsq                = t*t
      onePTSq            = 1 + tsq
      oneOverOnePTSq     = 1 / onePTSq

  cells (Sphere1D msh) = msh

  cell (Sphere1D msh) cidx = msh ! (idx cidx)

  cellAcross _ c Lo = c - 1
  cellAcross _ c Hi = c + 1

-- | Check if the ray contacts the inner sphere.
contactInner :: Position -> Position -> FP -> Bool -> Bool
contactInner (Position r) (Position rlow) tanTheta thetaLTpiOver2 =
    not thetaLTpiOver2      &&  -- moving toward inner
    rlow > 0                &&
    not (b ~==~ 1.0)        &&  -- not _on_ the inner sphere
    tanTheta <= tanThetaLim     -- sufficiently low angle
  where
    b           = r / rlow
    tanThetaLim = sqrt (1 / (b*b - 1))

-- TODO: Add comment and reference
pickPoint :: FP -> Rnd FP
pickPoint r = liftM (\ rs -> r * L.maximum rs) (randoms 3)

findCell :: Sphere1D -> FP -> CellIdx
findCell (Sphere1D msh) r = convert (binarySearchBy cmp r msh)
  where
    cmp :: FP -> Cell -> Ordering
    cmp r (Cell { lowB = Position l, highB = Position h })
      | r < l     = LT  -- TODO: check inclusive/exclusive bounds
      | r >= h    = GT  -- TODO: dito
      | otherwise = EQ

    convert :: Maybe Int -> CellIdx
    convert Nothing  = error "Sphere1D.findCell: malformed mesh"
    convert (Just n) = CellIdx n

rmax :: Sphere1D -> FP
rmax (Sphere1D msh) = pos . highB . V.last $ msh

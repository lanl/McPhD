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

-- | form mesh from sublist of cells and user limits. Also returns the number of
-- cells not used from the start of the list.
mkMesh :: [Cell] -> FP -> FP -> (Sphere1D, Int)
mkMesh clls llim ulim = (Sphere1D $ V.fromList cellsInBounds, ndropped)
  where cellsInBounds = rebound . fst . L.span leUL $ snd (L.break geLL clls)
        geLL, leUL :: Cell -> Bool
        geLL (Cell {highB = Position hir}) = hir >= llim
        leUL (Cell {lowB  = Position lor}) = lor <= ulim
        ndropped = L.length $ fst (L.break geLL clls)

-- | impose boundary conditions on a list of cells: reflective at lowermost,
-- vacuum at uppermost. Seems really clumsy...
rebound :: [Cell] -> [Cell]
rebound []     = []
rebound [c]    = [c {lowBC = Refl, highBC = Vac}]
rebound (c:cs) = lbc' : (L.init cs) L.++ [ubc']
  where lbc' = c {lowBC = Refl}
        ubc' = (L.last cs) {highBC = Vac}

instance Mesh Sphere1D where

  sampleDirectionIso _ = liftM (\ x -> Direction (2 * x - 1)) random

  samplePositionInCell _ cll = do
    xi <- random
    let ri3      = ri * ri * ri
        ro3      = ro * ro * ro
        (ri, ro) = (pos . lowB $ cll, pos . highB $ cll)
        psn      = Position ( (ri3 + (ro3 - ri3) * xi) ** (1/3) )
    return psn

  samplePosition msh = do
    r <- pickPoint (rmax msh)
    let psn = Position r
        cll = findCell msh r
    return (psn, cll)

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
      xterm1 =          r *     tsq * oneOverOnePTSq
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

  cell (Sphere1D msh) cidx =
    case msh !? (idx cidx) of
      Nothing -> error $ "Sphere1D::cell failed on cidx " L.++ show cidx
                 L.++ "\n mesh dump: " L.++ show msh
      Just c -> c

  cellAcross _ cidx Lo = cidx - 1
  cellAcross _ cidx Hi = cidx + 1

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
findCell (Sphere1D msh) r = exMaybe (binarySearchBy cmp r msh)
  where
    cmp :: FP -> Cell -> Ordering
    cmp x (Cell { lowB = Position l, highB = Position h })
      | x < l     = LT  -- TODO: check inclusive/exclusive bounds
      | x >= h    = GT  -- TODO: dito
      | otherwise = EQ

    exMaybe :: Maybe Int -> CellIdx
    exMaybe Nothing  = error "Sphere1D.findCell: malformed mesh"
    exMaybe (Just n) = CellIdx n

rmax :: Sphere1D -> FP
rmax (Sphere1D msh) = pos . highB . V.last $ msh

ncells :: Sphere1D -> Int
ncells (Sphere1D v) = V.length v

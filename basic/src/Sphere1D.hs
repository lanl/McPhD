{- | Sphere1D implements a spherical, 1D mesh. The typical use (i.e. the only -
 - thing I've thought of) is Refl boundary condition at the left-most, Vac BC -
 - at the rightmost, and Transp everywhere else. The code supports other      -
 - notions, at least in places. To do: either make the code enforce the       -
 - RT...TV model, or else test many other situations.                         -}
module Sphere1D where

import Control.Arrow
import Data.List as L
import Data.Vector as V

import Cell
import Mesh
import Physical
import Search
import SoftEquiv

type VecCell = Vector Cell

data Sphere1D = Sphere1D (VecCell) deriving Show

-- | form mesh from sublist of cells and user limits. Also returns the
-- number of cells not used from the start of the container.
mkMesh :: VecCell -> FP -> FP -> (Sphere1D, Int)
mkMesh clls llim ulim = (Sphere1D cellsInBounds, ndropped)
  where cellsInBounds = rebound kept
        (pre, (kept, _post)) = second (V.span leUL) $ V.break geLL clls
        geLL, leUL :: Cell -> Bool
        geLL (Cell {highB = Position hir}) = hir >= llim
        leUL (Cell {lowB  = Position lor}) = lor <= ulim
        ndropped = V.length pre

-- | re-impose boundary conditions on a vector of cells: reflective at
-- lowermost, vacuum at uppermost, transparent in between. Necessary
rebound :: VecCell -> VecCell
rebound v | V.length v == 0 = empty
          | V.length v == 1 = let c' = (v ! 0) {lowBC = Refl, highBC = Vac}
                              in singleton c'
          | otherwise       = generate n (genCell v n)
                                where n = V.length v

genCell :: VecCell -> Int -> Int -> Cell
genCell v _ 0 = (v!0) {lowBC = Refl}
genCell v n i | i < n = v!i
              | otherwise = (v ! i) {highBC = Vac}

-- rebound [c]    = [c {lowBC = Refl, highBC = Vac}]
-- rebound (c:cs) = lbc' : L.init cs L.++ [ubc']
--   where lbc' = c {lowBC = Refl}
--         ubc' = (L.last cs) {highBC = Vac}

instance Mesh Sphere1D where

  sampleDirectionIso _ (URD xi) = Direction (2 * xi - 1)

  samplePositionInCell _ cll (URD xi) =
    let ri3      = ri * ri * ri
        ro3      = ro * ro * ro
        (ri, ro) = (pos . lowB $ cll, pos . highB $ cll)
        psn      = Position ( (ri3 + (ro3 - ri3) * xi) ** (1/3) )
    in psn

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
    case msh !? idx cidx of
      Nothing -> error $ "Sphere1D.cell: failed on cidx " L.++ show cidx
                 L.++ "\n  mesh dump: " L.++ show msh
      Just c -> c

  cellAcross m cidx Lo = case (lowBC $ cell m cidx) of
                           Refl   -> cidx
                           Transp -> cidx - 1
                           Vac    -> -1
  cellAcross m cidx Hi = case (highBC $ cell m cidx) of
                           Refl   -> cidx
                           Transp -> cidx + 1
                           Vac    -> -1

  -- | compute new coordinate & direction after travelling a
  -- distance d along a given direction.
  -- newCoord :: Sphere1D -> Position -> Direction -> Distance ->
  --             (Position,Direction)
  newCoord _ (Position r) (Direction o) (Distance d) =
    (Position newR, Direction newO)
      where newR  = sqrt (newX * newX + newY * newY)
            newX  = r + d * o
            newY  = d * s
            theta = acos o
            s     = sin theta
            newO  = cos (theta - asin (d / newR * s))

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

findCell :: Sphere1D -> FP -> CellIdx
findCell (Sphere1D msh) r = exMaybe (binarySearchBy cmp r msh)
  where
    cmp :: FP -> Cell -> Ordering
    cmp x (Cell { lowB = Position l, highB = Position h })
      | x < l     = LT  -- TODO: check inclusive/exclusive bounds
      | x >= h    = GT  -- TODO: dito
      | otherwise = EQ

    exMaybe :: Maybe Int -> CellIdx
    exMaybe = maybe (error "Sphere1D.findCell: malformed mesh") CellIdx

rmax :: Sphere1D -> FP
rmax (Sphere1D msh) = pos . highB . V.last $ msh

ncells :: Sphere1D -> Int
ncells (Sphere1D v) = V.length v


-- testMesh :: Sphere1D
-- testMesh = Sphere1D (V.fromList [Cell (Position 1) (Position 2)
--                                       Transp Transp m1
--                                 ,Cell (Position 2) (Position 3)
--                                       Transp Transp m1])
--   where m1 = Material (Velocity 0) (Temperature 1) (Density 1) (NDensity 1)
--              (NDensity 0)

-- End of File

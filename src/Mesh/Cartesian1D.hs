{-# LANGUAGE TypeFamilies #-}

module Mesh.Cartesian1D where

import Data.Sequence as Seq
import Data.Vector.V2

import NormalizedValues
import Space.Cartesian1D
import Mesh.Classes
import Approx

type C1Cell = Int
data C1Dir  = Negative | Positive

data Cartesian1DMesh = Cartesian1DMesh
                       {
                         coords  :: Seq Double
                       , low_bc  :: BoundaryCondition
                       , high_bc :: BoundaryCondition
                       } deriving Show


cellBound :: Cartesian1DMesh -> C1Cell -> C1Dir -> Double
cellBound mesh cell Negative = coords mesh `Seq.index` cell
cellBound mesh cell Positive = coords mesh `Seq.index` (cell+1)

cellBoundsTest :: (Double -> Double -> Bool)
                  -> Cartesian1D
                  -> (Double, Double)
                  -> Bool
cellBoundsTest comp location (xMin, xMax) =
  let x       = pos location
      cos_dir = v2x . normalized_value $ dir location
  in ((x > xMin) || ( (x `comp` xMin) && cos_dir >= 0)) &&
     ((x < xMax) || ( (x `comp` xMax) && cos_dir < 0))


inCellTest :: (Double -> Double -> Bool)
              -> Cartesian1DMesh
              -> C1Cell
              -> Cartesian1D
              -> Bool
inCellTest comp mesh cell location =
  let xMin = cellBound mesh cell Negative
      xMax = cellBound mesh cell Positive
  in cellBoundsTest comp location (xMin, xMax)

instance Mesh Cartesian1DMesh where
  type MeshCell Cartesian1DMesh  = C1Cell
  type MeshFace Cartesian1DMesh  = C1Dir
  type MeshSpace Cartesian1DMesh = Cartesian1D

  size mesh = (Seq.length $ coords mesh) - 1

  cell_find mesh location =
    let bounds       = coords mesh
        _ :< boundsR = viewl bounds
        pairs        = Seq.zip bounds (boundsR)
    in Seq.findIndexL (cellBoundsTest (==) location) pairs

  cell_neighbor mesh cell Negative
    | cell == 0 = boundary2neighbor $ low_bc mesh
    | otherwise = Cell (cell-1)
  cell_neighbor mesh cell Positive
    | cell == (size mesh)-1 = boundary2neighbor $ high_bc mesh
    | otherwise = Cell (cell+1)

  cell_neighbors mesh cell = [(Negative, cell_neighbor mesh cell Negative),
                              (Positive, cell_neighbor mesh cell Positive)]

  is_in_cell        = inCellTest (==)
  is_approx_in_cell = inCellTest (~==)
  is_in_mesh mesh location  = cellBoundsTest (==) location (left, right)
      where (left :< rest)  = viewl $ coords mesh
            (  _  :> right) = viewr rest

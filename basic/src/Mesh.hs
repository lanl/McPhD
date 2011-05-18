module Mesh where

import Data.Vector as V

import Cell
import Event
import Material
import PRNG
import Physical

class Mesh m where
  samplePosition       :: m -> Rnd (Position, CellIdx)
  samplePositionInCell :: m -> Cell -> Rnd Position
  sampleDirection      :: m -> Rnd Direction
  distanceToBoundary   :: m -> CellIdx -> Position -> Direction -> (Distance, Face)
  cells                :: m -> Vector Cell
  cell                 :: m -> CellIdx -> Cell
  cellAcross           :: m -> CellIdx -> Face -> CellIdx

-- | Returns the maximum index of the vector of cells.
nrCells :: Mesh m => m -> Int
nrCells = V.length . cells

-- QUESTION: It might be better to return the length instead.

material :: Mesh m => m -> CellIdx -> Material
material msh (CellIdx cidx) = mat (cells msh ! cidx)

boundaryType :: Mesh m => m -> CellIdx -> Face -> BoundaryCondition
boundaryType msh (CellIdx cidx) Lo = lowBC  (cells msh ! cidx)
boundaryType msh (CellIdx cidx) Hi = highBC (cells msh ! cidx)

toBoundaryEvent :: BoundaryCondition -> BoundaryEvent
toBoundaryEvent Vac    = Boundary Escape
toBoundaryEvent Refl   = Boundary Reflect
toBoundaryEvent Transp = Boundary Transmit

boundaryEvent :: Mesh m => m -> CellIdx -> BoundaryEvent
boundaryEvent msh cidx d face =
  toBoundaryEvent (boundaryType msh cidx face) d face



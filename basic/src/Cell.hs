{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cell where

import Physical
import Material

data BoundaryCondition = Vac | Refl | Transp
  deriving (Show,Eq)

data Cell = Cell {
    lowB   :: !Position
  , highB  :: !Position
  , lowBC  :: !BoundaryCondition
  , highBC :: !BoundaryCondition
  , mat    :: !Material
  }
  deriving (Show, Eq)

data Face = Lo | Hi
  deriving (Eq, Show)


data CellGeom   = CellGeom Position Position BoundaryCondition BoundaryCondition
  deriving (Show, Eq)


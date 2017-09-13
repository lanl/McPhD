{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
module Cell where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import Physical
import Material

data BoundaryCondition = Vac | Refl | Transp
  deriving (Show,Eq,Generic)

instance NFData BoundaryCondition

data Cell = Cell {
    lowB   :: !Position
  , highB  :: !Position
  , lowBC  :: !BoundaryCondition
  , highBC :: !BoundaryCondition
  , mat    :: !Material
  }
  deriving (Show, Eq, Generic)

instance NFData Cell

data Face = Lo | Hi
  deriving (Eq, Show)


data CellGeom   = CellGeom Position Position BoundaryCondition BoundaryCondition
  deriving (Show, Eq)


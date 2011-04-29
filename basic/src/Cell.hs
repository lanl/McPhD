module Cell where

import Physical
import Material

data BoundaryCondition = Vac | Refl | Transp | None
  deriving Show

data Cell = Cell {
    lowB   :: Position
  , highB  :: Position
  , lowBC  :: BoundaryCondition
  , highBC :: BoundaryCondition
  , mat    :: Material 
  }
  deriving Show

data Face = Lo | Hi 
  deriving (Eq, Show)

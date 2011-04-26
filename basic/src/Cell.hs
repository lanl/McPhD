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
  , mat    :: Material -- QUESTION: ok to include here?
  }
  deriving Show

data Face = Lo | Hi -- QUESTION: all 1D, how to generalize
  deriving (Eq, Show)

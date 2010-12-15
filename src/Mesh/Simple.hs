{- | A uniform Cartesian Mesh in three dimensions. Min coordinate values are zero.
-}
module Mesh.Simple where

import Data.Vector.V3
import Data.Vector.Class

import Space
    
-- | Size of the mesh in cells, along each axis 
data CellIndex = CellIndex { nx :: Integer, ny :: Integer, nz :: Integer } deriving (Show, Eq)

-- | The Cartesian directions in 3D. Used to identify faces of a cell.
data Mesh_Direction = Negative_X 
                    | Positive_X 
                    | Negative_Y 
                    | Positive_Y 
                    | Negative_Z 
                    | Positive_Z deriving (Eq, Show, Ord)

-- | Types for indexing cells in the mesh
data Cell = Index CellIndex | Void deriving (Show, Eq)
make_cell :: [Integer] -> Cell
make_cell (x: (y: (z:_))) = Index (CellIndex x y z)
make_cell _ = error "Not enough indices for a cell"


-- | Type for indexing faces in the mesh. Faces are the boundaries
-- between cells, or a cell and the edge of the computational domain
type Face = (Cell, Mesh_Direction)

data SimpleMesh = SimpleMesh { size    :: CellIndex -- ^ Size of the mesh in cells
                             , dim     :: Vector3   -- ^ Dimensions of each cell.
                             } deriving Show

mesh_size :: SimpleMesh -> Integer
mesh_size (SimpleMesh (CellIndex nx ny nz) _ ) = nx*ny*nz

-- | Find the mesh cell containing a given position
cell :: SimpleMesh -> Position -> Cell
cell mesh position = make_cell $ map floor $ vunpack (vzip (/) (pos position) (dim mesh))

-- | Find the cell on the other side of a given face. Returns Nothing
-- the the face does not belong to the given cell.
next :: SimpleMesh -> Cell -> Face -> Maybe Cell
next _ Void _ = Nothing
next _ cell@(Index (CellIndex ix iy iz)) (cell', mesh_direction) 
  | not (cell == cell') = Nothing
  | otherwise = undefined

    


  

-- | Streaming distance from given position in the given direction, to
-- the first-encountered face of cell.
escape_distance :: SimpleMesh -> Cell -> Position -> Direction -> (Distance, Face)
escape_distance mesh cell position direction = undefined





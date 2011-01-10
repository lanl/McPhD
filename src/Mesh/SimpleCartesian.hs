{- | A uniform Cartesian Mesh in three dimensions. Min coordinate values are zero.
-}
module Mesh.SimpleCartesian where

import Mesh.Classes
import Space
    
import Data.Vector.V3
import Data.Vector.Class

-- | Size of the mesh in cells, along each axis 
data CellIndex = CellIndex { nx :: Integer, ny :: Integer, nz :: Integer } deriving (Show, Eq)

-- | An inequality operator for testing membership in a mesh.
(<=//) :: CellIndex -> CellIndex -> Bool
(<=//) (CellIndex nx ny nz) (CellIndex nx' ny' nz') = and [(nx <= nx'), (ny <= ny'), (nz <= nz')]

(>=//) :: CellIndex -> CellIndex -> Bool
(>=//) a b = b <=// a

-- | The Cartesian directions in 3D. Used to identify faces of a cell.
data Mesh_Direction = Negative_X 
                    | Positive_X 
                    | Negative_Y 
                    | Positive_Y 
                    | Negative_Z 
                    | Positive_Z deriving (Eq, Show, Ord)
                                          
next_cell :: CellIndex -> Mesh_Direction -> CellIndex
next_cell (CellIndex nx ny nz) d = 
  case d of
    Negative_X -> CellIndex (nx-1) ny nz
    Positive_X -> CellIndex (nx+1) ny nz
    Negative_Y -> CellIndex nx (ny-1) nz
    Positive_Y -> CellIndex nx (ny+1) nz
    Negative_Z -> CellIndex nx ny (nz-1)
    Positive_Z -> CellIndex nx ny (nz+1)
    
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





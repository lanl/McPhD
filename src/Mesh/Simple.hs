module Mesh.Simple where

-- A uniform Cartesian Mesh in three dimensions. Min coordinate values are zero.

import Data.Vector.V3
import Data.Vector.Class

import Space
    
-- | Size of the mesh in cells, along each axis 
data MeshSize = MeshSize { nx :: Integer, ny :: Integer, nz :: Integer }

data Direction = Negative_X 
               | Positive_X 
               | Negative_Y 
               | Positive_Y 
               | Negative_Z 
               | Positive_Z deriving (Eq, Show, Ord)

-- | Types for indexing cells in the mesh
data Cell = Cell Integer Integer Integer
make_cell :: [Integer] -> Cell
make_cell (x: (y: (z:_))) = Cell x y z

-- | Type for indexing faces in the mesh. Faces are the boundaries
-- between cells, or a cell and the edge of the computational domain
type Face = (Cell, Mesh.Simple.Direction)

data SimpleMesh = SimpleMesh { size    :: MeshSize -- ^ Size of the mesh in cells
                             , dim     :: Vector3  -- ^ Either size of the mesh or size of the cell
                             }

mesh_size :: SimpleMesh -> Integer
mesh_size (SimpleMesh (MeshSize nx ny nz) _ ) = nx*ny*nz

cell :: SimpleMesh -> Position -> Cell
cell mesh position = make_cell $ map floor $ vunpack (vzip (/) (pos position) (dim mesh))

next :: SimpleMesh -> Cell -> Face -> Maybe Cell
next = undefined

-- | Streaming distance from @a@ to the first-encountered face of @Cell@. 
distance :: SimpleMesh -> Cell -> a -> (Distance, Face)
distance = undefined    



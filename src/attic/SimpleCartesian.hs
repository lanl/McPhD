{- | A uniform Cartesian Mesh in three dimensions. Min coordinate values are zero.
   For now, single domain, all boundaries lead to the void.
-}
module Mesh.SimpleCartesian where

import Space.Space3DCartesian

import Data.Vector.V3
import Data.Vector.Class
import Data.Ix

-- | Size of the mesh in cells, along each axis
data CellIndex = CellIndex { nx :: Int, ny :: Int, nz :: Int }
               deriving (Show, Eq, Ord, Ix)

toTuple :: CellIndex -> (Int, Int, Int)
toTuple (CellIndex nx ny nz) = (nx,ny,nz)

fromTuple :: (Int, Int, Int) -> CellIndex
fromTuple (x,y,z) = CellIndex x y z

fromList :: [Int] -> Maybe CellIndex
fromList (x:y:z:_) = Just $ CellIndex x y z
fromList _ = Nothing


-- | The Cartesian directions in 3D. Used to identify faces of a cell.
data Local_Face = Negative_X
                | Positive_X
                | Negative_Y
                | Positive_Y
                | Negative_Z
                | Positive_Z deriving (Eq, Show)

next_index :: CellIndex -> Local_Face -> CellIndex
next_index (CellIndex nx ny nz) face =
  case face of
    Negative_X -> CellIndex (nx-1) ny nz
    Positive_X -> CellIndex (nx+1) ny nz
    Negative_Y -> CellIndex nx (ny-1) nz
    Positive_Y -> CellIndex nx (ny+1) nz
    Negative_Z -> CellIndex nx ny (nz-1)
    Positive_Z -> CellIndex nx ny (nz+1)

-- | Type for indexing faces in the mesh. Faces are the boundaries
-- between cells, or a cell and the edge of the computational domain
data Face = Face { cell::CellIndex, local::Local_Face } deriving (Show)

nextIndex :: Face -> CellIndex
nextIndex face = next_index (cell face) (local face)


data SimpleMesh = SimpleMesh { maxIndex :: CellIndex -- ^ Max cell index in each dir.
                             , cellDim  :: Vector3   -- ^ Dimensions of each cell.
                             } deriving Show

meshSize :: SimpleMesh -> Int
meshSize mesh = let CellIndex x y z = maxIndex mesh in x*y*z

inMesh :: SimpleMesh -> CellIndex -> Bool
inMesh mesh index = inRange (CellIndex 0 0 0, maxIndex mesh) index


data Cell = Local CellIndex
          | Void deriving (Eq, Show)

-- | Convert a CellIndex to a Cell.
toCell :: SimpleMesh
          -> CellIndex
          -> Cell
toCell mesh index = if inMesh mesh index then (Local index)
                    else Void

-- | Find the cell on the other side of a given face. Returns Nothing if the face
-- is on the boundary of the mesh
nextCell :: SimpleMesh -> Face -> Cell
nextCell mesh face = toCell mesh (nextIndex face)





-- | Find the mesh cell containing a given position
findCell :: SimpleMesh -> Position -> Maybe CellIndex
findCell mesh position =
  (fromList $ map floor $ vunpack ((pos position) / (cellDim mesh)))


faceOnCell :: Face -> Cell -> Bool
faceOnCell _ Void = False
faceOnCell (Face cell _) (Local cell') = cell == cell'

{- | A uniform Cartesian Mesh in three dimensions. Min coordinate values are zero.
   For now, single domain, all boundaries lead to the void.
-}
module Mesh.SimpleCartesian where

import Space3DCartesian

import Data.Vector.V3
import Data.Vector.Class
import Data.Maybe
import Data.Number.PartialOrd
import Control.Monad
import Control.Applicative
import Data.Ix


compare_all :: [Ordering] -> Maybe Ordering
compare_all comps
    | all (==EQ) comps = Just EQ
    | all (==LT) comps = Just LT
    | all (==GT) comps = Just GT
    | otherwise = Nothing

-- | Size of the mesh in cells, along each axis
data CellIndex = CellIndex { nx :: Integer, ny :: Integer, nz :: Integer }
	       deriving (Show, Eq, Ord, Ix)

toList :: CellIndex -> [Integer]
toList (CellIndex nx ny nz) = [nx,ny,nz]

fromList :: [Integer] -> Maybe CellIndex
fromList (x:y:z:[]) = Just $ CellIndex x y z
fromList _ = Nothing

instance PartialOrd CellIndex where
    cmp a b = let comps = liftA2 compare (toList a) (toList b) in
	      compare_all comps

minimumIndex :: CellIndex
minimumIndex = CellIndex 0 0 0

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


-- | Types for indexing cells in the mesh
data Cell = Local CellIndex
	  | Void             -- ^ Used to indicate space beyond the mesh
	    deriving (Show, Eq)


-- | Type for indexing faces in the mesh. Faces are the boundaries
-- between cells, or a cell and the edge of the computational domain
data Face = Face Cell Local_Face

data SimpleMesh = SimpleMesh { maxIndex :: CellIndex -- ^ Max cell index in each dir.
			     , cellDim  :: Vector3   -- ^ Dimensions of each cell.
			     } deriving Show

meshSize :: SimpleMesh -> Integer
meshSize mesh = let CellIndex x y z = maxIndex mesh in x*y*z

inMesh :: SimpleMesh -> CellIndex -> Bool
inMesh mesh index = inRange (minimumIndex, maxIndex mesh) index

-- | Find the mesh cell containing a given position
findCell :: SimpleMesh -> Position -> Maybe Cell
findCell mesh position =
  let index = (fromList $ map floor $ vunpack ((pos position) / (cellDim mesh)))
  in liftM Local index

faceOnCell :: Face -> Cell -> Bool
faceOnCell _ Void = False
faceOnCell (Face cell' _) cell = cell == cell'

-- | Find the cell on the other side of a given face. Returns Nothing
-- the the face does not belong to the given cell.
nextCell :: SimpleMesh -> Cell -> Face -> Maybe Cell
nextCell mesh cell face
  | not (face `faceOnCell` cell) = Nothing
  | otherwise = undefined

-- | Streaming distance from given position in the given direction, to
-- the first-encountered face of cell.
escapeDistance :: SimpleMesh -> Cell -> Position -> Direction -> (Distance, Face)
escapeDistance mesh cell position direction = undefined

import Data.Array.IArray
import Data.Ix

-- An index type for coordinates.

data Coord = X | Y | Z deriving (Show, Eq, Ord, Ix)
myarray = listArray (X,Z) ['x', 'y','z'] :: Array Coord Char

newtype InSpace v = InSpace (Array Coord v) deriving (Show, Eq, Ord)

toInSpace :: (a,a,a) -> InSpace a
toInSpace (x,y,z) = InSpace $ listArray (X,Z) [x,y,z]


-- I can make a newtype wrapper of a tuple an instance if Ix.
newtype CellIndex = CellIndex { get_tuple :: (Int, Int, Int) }
		  deriving (Show, Eq, Ord, Ix)

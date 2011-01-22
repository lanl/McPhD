import Data.Array.IArray

import Data.Ix

data Coord = X | Y | Z deriving (Show, Eq, Ord, Ix)

myarray = array (X,Z) [(X, 'x'), (Y,'y'), (Z,'z')] :: Array Coord Char

newtype Indexed v = Indexed (Array Coord v) deriving (Show)

i :: Indexed Int
i = Indexed $ listArray (X,Z) [10,10,10]
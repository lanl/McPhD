{-# LANGUAGE TypeSynonymInstances  #-}
import Data.Functor
import Data.Array.IArray
import Data.Ix

-- An index type for coordinates.
data Coord = X | Y | Z deriving (Show, Eq, Ord, Ix)

-- I can use this to index into arrays. For example, here's an array which stores a name for each one.
myarray = listArray (X,Z) ['x', 'y', 'z'] :: Array Coord Char

-- Here's a newtype for arrays indexed over these coordinates.
newtype InSpace v = InSpace (Array Coord v) deriving (Show, Eq, Ord)

-- A function for converting tuples of values to InSpace arrays
toInSpace :: (a,a,a) -> InSpace a
toInSpace (x,y,z) = InSpace $ listArray (X,Z) [x,y,z]

-- A class of generalized indices. These are things we want to convert
-- to/from triples for conversions.
class GenIndex a where
    toTuple   :: a -> (Int, Int, Int)
    fromTuple :: (Int, Int, Int) -> a

-- Not sure how to write instances of this more general class.
class GenTuple a where
    toTuple'   :: a -> (b,b,b)
    fromTuple' :: (b,b,b) -> a



-- I can almost make InSpace an index type for Arrays automatically.
newtype SpaceIndex = SpaceIndex { fromSpaceIndex :: (InSpace Int) }
		   deriving (Show, Eq, Ord)

-- Make it a generalized index for easy conversion
instance GenIndex SpaceIndex where
    toTuple   = toTuple . fromSpaceIndex
    fromTuple = SpaceIndex . toInSpace

-- We need to make it an instance of Ix
instance Ix SpaceIndex where
    range (a,b)     = map (SpaceIndex . fromTuple) $ range (toTuple a, toTuple b)
    index (a,b) i   = index   (toTuple a, toTuple b) (toTuple i)
    inRange (a,b) i = inRange (toTuple a, toTuple b) (toTuple i)

-- Define Pair class and make an instance of Functor to apply functions to both members.
newtype Pair a = Pair { fromPair :: (a,a) } deriving (Show)
instance Functor Pair where
    fmap f (Pair p) = Pair (f (fst p), f (snd p))

-- Define an ad-hoc version of fmap for 2-tuples.
onPair :: (a -> b) -> (a,a) -> (b,b)
onPair f p = (f $ fst p, f $ snd p)



-- We can also use a type synonym for the space index.
type SpaceIndexType = InSpace Int

instance GenIndex SpaceIndexType where
    toTuple (InSpace s) = (s!X, s!Y, s!Z)
    fromTuple = toInSpace

-- Make an instance of Ix. Here, we use three different ways of dealing with pairs
instance Ix SpaceIndexType where
    range p   = map fromTuple $ range . fromPair $ toTuple <$> Pair p     -- Using the Pair type for range.
    index p i = index (onPair toTuple p) (toTuple i)                      -- Pattern matching on the tuple.
    inRange p i = inRange (toTuple (fst p), toTuple (snd p)) (toTuple i)  -- Extracting data from the tuple

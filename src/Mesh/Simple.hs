{-| Simple mesh, dividing space into parallelepiped cells stacked in neat rows and columns
-}
module Mesh.Simple where

import Data.Vector.V3

import Space
import Particle.Simple
    
-- | Size of the mesh in cells, along each axis (FIXME: correct?). 
-- TODO: maybe, needs less generic name in order not to pollute the namespace
data Size = Size { nx :: Integer, ny :: Integer, nz :: Integer }

data SimpleMesh = SimpleMesh { size :: Size -- ^ Size of the mesh in cells
                             , dim ::Vector3 -- ^ Either size of the mesh or size of the cell
                             , opacity :: [Double] -- ^ TODO: what is this?
                             }

-- | Cell number (FIXME: ?)
data Cell = Integer

-- | Face separates two cells with given numbers
type Face = (Cell,Cell)

-- | Find the cell bounding the given position
cell :: SimpleMesh -> Position -> Cell
cell = undefined

-- | Which cell lies behind given face
next :: SimpleMesh -> Cell -> Face -> Maybe Cell
next = undefined

-- | TODO: ???
get :: SimpleMesh -> Cell -> Double
get = undefined

-- | Distance from @a@ to the nearest(?) 'Face'. FIXME: correct?
distance :: SimpleMesh -> Cell -> a -> (Distance, Face)
distance = undefined    

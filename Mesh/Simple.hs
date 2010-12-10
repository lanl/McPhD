module Mesh.Simple where

import Data.Vector.V3

import Particles.Data    
import Particles.Simple
    
data Size = Size { nx :: Integer, ny :: Integer, nz :: Integer }

data SimpleMesh = SimpleMesh { size :: Size, dim ::Vector3 , opacity :: [Double] }

data Cell = Integer
type Face = (Cell,Cell)

cell :: SimpleMesh -> Position -> Cell
cell = undefined

next :: SimpleMesh -> Cell -> Face -> Maybe Cell
next = undefined

get :: SimpleMesh -> Cell -> Double
get = undefined

distance :: SimpleMesh -> Cell -> a -> (Distance, Face)
distance = undefined    
    
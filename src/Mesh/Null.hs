module Mesh.Null where

data Position  = Position
data Direction = Direction
data Face      = Face
data Cell      = Cell
data Field     = Field
data Distance  = Distance

data NullMesh = NullMesh

cell :: NullMesh -> Position -> Cell
cell _ _ = Cell

next :: NullMesh -> Cell -> Face -> Maybe Cell
next _ _ _ = Just Cell

get :: NullMesh -> Cell -> Field
get _ _ = Field

distance :: NullMesh -> Cell -> a -> (Distance, Face)
distance _ _ _ = (Distance, Face)


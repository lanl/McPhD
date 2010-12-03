{-# OPTIONS_GHC -XMultiParamTypeClasses #-}

module Mesh.NullMesh where

import Mesh.Classes

data Space = Space
data Direction = Direction
data Face = Face
data Cell = Cell
data Field = Field
data Distance = Distance

data NullMesh = NullMesh
instance Mesh NullMesh Space Direction Distance Field Cell Face where
  cell _ _ = Cell
  next _ _ = Cell
  get  _ _ = Field
  distance _ _ _ _ = (Distance, Face)


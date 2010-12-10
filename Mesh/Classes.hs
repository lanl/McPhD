{-# OPTIONS_GHC -XMultiParamTypeClasses -XTypeFamilies #-}

module Mesh.Classes (Mesh (..)) where

class Mesh m point direction distance fields cell face where
  cell     :: m -> point -> cell
  next     :: m -> face -> cell
  get      :: m -> cell -> fields
  distance :: m -> cell -> point -> direction -> (distance, face)
  
class MeshFamily m where
    data Space m     :: *
    data Partition m :: *
    data Fields m    :: *
    


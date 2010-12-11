{-# OPTIONS_GHC  -XTypeFamilies #-}

-- An exploratory typeclass for Meshes

module Mesh.Classes (MeshFamily (..)) where

class MeshFamily m where
    data Space m     :: *
    data Partition m :: *
    data Data m      :: *
    


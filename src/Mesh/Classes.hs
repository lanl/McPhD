{-# OPTIONS_GHC  -XTypeFamilies #-}

{-| An exploratory typeclass for Meshes.
-}
module Mesh.Classes (MeshFamily (..)) where

-- | FIXME: does the processing of different meshes have enough in common to warrant a typeclass
-- that would provide some useful abstraction?
class MeshFamily m where
    data Space m     :: *
    data Partition m :: *
    data Data m      :: *
    


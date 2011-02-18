-- MeshBase.hs
-- T. M. Kelley
-- Feb 15, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module MeshBase (Mesh(..))
    where

-- import SoftEquiv
import Physical
-- import Event
import Cell 
-- import Data.Array (bounds)

-- | Mesh properties for each cell
data Mesh = Sphere1D { mesh :: Array CellIdx CellProperties } -- assume cells ordered by radius
          | Cart3D   { mesh :: Array CellIdx CellProperties }
          | Cart1D   { mesh :: Array CellIdx CellProperties }
            deriving (Show)

-- version
-- $Id$

-- End of file

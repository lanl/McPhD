-- | A module for building a specific particle transport mini-application
module Main where

import Space.Spherical1D
import Mesh.Spherical


-- | Events
data Event = Scatter | CellFace | Esacpe | Census

isFinal :: Event -> Bool
isFinal Escape {} = True
isFinal Census {} = True
isFinal _         = False

isContinuing = not . isFinal


-- A Mesh


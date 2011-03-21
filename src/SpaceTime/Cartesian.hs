module SpaceTime.Cartesian where

import SpaceTime.Classes
import Data.Vector.Class

data Cartesian v = Cartesian { position :: v, direction :: v }
                 deriving (Eq, Show)
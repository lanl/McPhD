module Particles.Null where

import Particles.Classes

data Position = Position
data Direction = Direction

data NullParticle = NullParticle

position :: NullParticle -> Position
position _ = Position

direction :: NullParticle -> Direction
direction _ = Direction


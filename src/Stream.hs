{-# LANGUAGE TypeFamilies #-}

module Stream where

import Particle.Classes
import Data.List


-- | Prototype events type without associated data
data Event = Scatter | CellFace | Escape | Census

isFinal :: Event -> Bool
isFinal Escape {} = True
isFinal Census {} = True
isFinal _         = False

step :: (Mesh m, Particle p) => m -> p -> (Event, p)
step = undefined

stream :: Mesh -> Particle -> [(Event, Particle)]
stream mesh particle = stream' mesh particle []
    where stream' :: Mesh -> Particle -> [(Event, Particle)] -> [(Event, Particle)]
          stream' mesh particle events = 

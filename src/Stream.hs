{-# LANGUAGE TypeFamilies #-}

module Stream where

import Mesh.Classes

-- | Prototype events type without associated data
data Event = Scatter | CellFace | Escape | Census

isFinal :: Event -> Bool
isFinal Escape {} = True
isFinal Census {} = True
isFinal _         = False

isContinuing = not . isFinal

stream :: (p -> (e,p)) -> (e -> Bool) -> p -> [(e, p)]
stream stepper continue p =
  let (e, p') = stepper p
  in  (e, p') : if continue e then stream stepper continue p' else []

{-| Generic notion of streaming: we take something, simulate, and return generated event and new something
-}
module Streaming where

import Events.Event

-- | TODO: name like 'step' seems more appropriate, like in 'simulation step'.
-- And class could be named Steppable. And module - Steppable.hs
class Stream s where
  stream :: s -> Maybe (Event, s)

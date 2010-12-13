module Streaming where

import Events.Event
import Data.List

class Stream s where
  stream :: s -> Maybe (Event, s)

makeStream :: (Stream s) => s -> [Event]
makeStream s = unfoldr stream s
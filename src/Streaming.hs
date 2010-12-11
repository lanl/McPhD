module Streaming where

import Events.Event

class Stream s where
  stream :: s -> Maybe (Event, s)

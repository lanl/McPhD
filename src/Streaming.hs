{-# OPTIONS_GHC -XTypeFamilies #-}

module Streaming where

import Events.Event
import Data.List

class Stream s where
    type Particle :: *
    stream :: s -> Maybe (Event Particle, s)

makeStream :: (Stream s) => s -> [Event Particle]
makeStream s = unfoldr stream s
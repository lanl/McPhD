{-# OPTIONS_GHC -XTypeFamilies #-}

module Stream where

import Events.Event
import Data.List

class Steppable s where
    type Particle :: *
    step :: s -> Maybe (Event Particle, s)

type Stream = [Event Particle]

makeStream :: (Steppable s) => s -> Stream
makeStream s = unfoldr step s

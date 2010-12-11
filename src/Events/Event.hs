module Events.Event where

data Motion   = Stream Double deriving Show
data Endpoint = Scatter | Escape | Reflect deriving Show

data Event = Event Motion Endpoint | NullEvent deriving Show
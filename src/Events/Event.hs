{-| Datatypes for events generated during simulation.
-}
module Events.Event where

-- | TODO: Why constructor and datatype have such drastically different names?
data Motion   = Stream Double deriving Show

-- | What has happened to the particle in the end
data Endpoint = Scatter | Escape | Reflect deriving Show

data Event = Event Motion Endpoint -- ^ Ordinary event consists of series of motions (FIXME: correct?) + final event
           | NullEvent -- ^ TODO: what is this? just for testing?
           deriving Show
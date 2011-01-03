module Tallies where

import RandomParticle

p0 = Momentum (Vector3 0 0 0)
m0 = Motion   (Vector3 0 0 0)

getEventData :: Event -> (Direction, Direction)
getEventData Event (Motion m) (Scatter p) = (m, p)
getEventData _ _ = (m0, p0)

getFinalState :: Event -> (Direction, RandomParticle)
getFinalState Event (Motion m) (Termination p) = (m, p)
getFinalState _ _ = undefined


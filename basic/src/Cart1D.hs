-- Cart1D.hs
-- T. M. Kelley
-- Feb 15, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Cart1D ( sampDir
              , sampPos
              , distToBdy)
    where 

import Physical
import Cell 
import MeshBase

sampDir :: RNG -> IO Direction
sampDir = undefined

sampPos :: Mesh -> RNG -> IO (Position,CellIdx)
sampPos = undefined

-- | Distance to boundary in cartesian 1D coordinates
{- Create a list of pairs of distance to high & low faces divided by direction
 - cosine in each dimension and the corresponding face. Filter list to 
 - remove the element with negative distance. This leaves one choice, which
 - we take. This all relies on a well-formed mesh, in which the bounds are 
 - monotonically increasing. -}
distToBdy :: Position -> Direction -> Position -> Position -> (FP, Face)
distToBdy psn drn b_low b_high = 
    let dnfs = [((xcomp lows)/(xcomp d),XLow),((xcomp highs)/(xcomp d),XHigh)]
        lows  = pos(psn - b_low)   --  -ve: chosen if omega < 0
        highs = pos(psn - b_high)  --  +ve: chosen if omega > 0
        d = dir drn
    in head (filter (\(a,_) -> a>=0) dnfs)




-- version
-- $Id$

-- End of file

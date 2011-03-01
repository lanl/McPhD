-- Cart3D.hs
-- T. M. Kelley
-- Feb 15, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Cart3D (sampPos
              ,sampDir
              ,across
              ,distToBdy)
    where

import Physical
import Cell 
import MeshBase
import Data.Function
import Data.List
 
sampDir :: FP -> FP -> FP -> Direction
sampDir = undefined

sampPos :: Mesh -> FP -> FP -> FP -> (Position,CellIdx)
sampPos = undefined

across :: CellIdx -> Face -> CellIdx
across = undefined

-- | Distance to boundary in cartesian 3D coordinates
{- Create a list of pairs of distance to high & low faces divided by direction
 - cosine in each dimension and the corresponding face. Filter list to 
 - remove elements with negative distance. Pass to helper function that
 - finds the pair with least distance. As in the 1D case, this relies on
 - a well-formed mesh, in which the bounds are monotonically increasing. 
 - If this function is called with Vector1's, there will be many divide-by-0's,
 - from a correctness perspective, it looks like the program will do the right
 - thing, but that may be a performance killer. -}
distToBdy :: Position -> Direction -> Position -> Position -> (FP, Face)
distToBdy psn drn b_low b_high = 
    let dnfs = [((xcomp lows)/(xcomp d),XLow),((xcomp highs)/(xcomp d),XHigh),
                ((ycomp lows)/(ycomp d),YLow),((ycomp highs)/(ycomp d),YHigh),
                ((zcomp lows)/(zcomp d),ZLow),((zcomp highs)/(zcomp d),ZHigh)]
        lows  = pos(psn - b_low)   --  -ve: chosen if omega < 0
        highs = pos(psn - b_high)  --  +ve: chosen if omega > 0
        d = dir drn
    in closestFace (filter (\(a,_) -> a>=0) dnfs)

closestFace :: [(FP,Face)] -> (FP,Face)
closestFace = minimumBy (compare `on` fst)


-- version
-- $Id$

-- End of file

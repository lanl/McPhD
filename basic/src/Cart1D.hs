-- Cart1D.hs
-- T. M. Kelley
-- Feb 15, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Cart1D ( sampDir
              , sampPos
              , across
              , distToBdy)
    where 

import Physical
import Cell 
import MeshBase
import Data.Array (bounds)

sampDir :: FP -> FP -> FP -> Direction
sampDir x1 _ _ = Direction . Vector1 $ 2.0 * x1 - 1.0

sampPos :: Mesh -> FP -> FP -> FP -> (Position,CellIdx)
sampPos msh x1 _ _ = (Position . Vector1 $ x, findCell msh x)
    where x = (hi-lo)*x1 + lo
          lo = xcomp . pos . lowB $ mesh msh ! f
          hi = xcomp . pos . highB $ mesh msh ! l
          (f,l) = bounds $ mesh msh

across :: CellIdx -> Face -> CellIdx
across c XLow = c - 1
across c XHigh = c + 1
across _ _ = error "Sphere1D.across only defined for XLow and XHigh boundaries"

-- b-search in the mesh
findCell :: Mesh -> FP -> CellIdx
findCell msh x =  impl msh x f l
    where impl msh x lo hi | lo == hi  = lo
                           | vlo <= x && vhi >= x = idx
                           | vlo >= x = impl msh x lo idx
                           | vhi <= x = impl msh x (idx+1) hi
                           where idx = (lo+hi) `quot` 2
                                 vlo = xcomp . pos . lowB  $ mesh msh ! idx
                                 vhi = xcomp . pos . highB $ mesh msh ! idx
          (f,l) = bounds $ mesh msh
          
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

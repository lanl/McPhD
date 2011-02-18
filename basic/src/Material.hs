-- Material.hs
-- T. M. Kelley
-- Jan 28, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Material (MatState(..)
                ,Material(..)
                -- ,simpleMat
                )
    where

import Numerical
import Physical
-- import Data.Array

-- | Material state in a cell
data MatState = MatState {
      sig_abs  :: !Opacity
    , sig_scat :: !Opacity
    , vel      :: !Velocity
    , temp     :: !Temperature
    } deriving (Show,Eq)


-- | Collection of material state information
newtype Material = Material {mat :: Array CellIdx MatState} 


-- version
-- $Id$

-- End of file

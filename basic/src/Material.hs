-- Material.hs
-- T. M. Kelley
-- Jan 28, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Material (MatState
                ,Material(..)
                ,sig_abs
                ,sig_scat
                ,Material.vel
                ,Material.temp
                ,simpleMat)
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


-- try tuning the scattering and absorption opacities in each cell
--                                           scat  abs
simpleMat :: Material
simpleMat  =  Material $ listArray (1,2) [ MatState (Opacity 2.0) (Opacity 0.2) (Velocity 0.0) (Temperature 1.0),
                                           MatState (Opacity 1.0) (Opacity 0.5) (Velocity 0.0) (Temperature 2)]


-- version
-- $Id$

-- End of file

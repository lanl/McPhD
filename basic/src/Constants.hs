-- SimpleConstants.hs
-- T. M. Kelley
-- Dec 08, 2010
-- (c) Copyright 2010 LANSLLC all rights reserved.

module Constants where

import Numerical

-- constants 

-- speed of light in vacuum
-- c    = 2.9979e10 :: FP    -- cm/sec
c :: FP 
c = 1.0 -- nice for testing

-- limits for event selection
tiny, huge :: FP
tiny = 1e-300    
huge = 1e+300     

-- version
-- $Id$

-- End of file

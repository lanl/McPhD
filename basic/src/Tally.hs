-- Tally.hs
-- T. M. Kelley
-- Jul 12, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Tally
  ( module Tally)
 where

#ifdef VECTOR_TALLY
import qualified TallyV as Tally
#else 
import qualified TallyIM as Tally
#endif

-- version
-- $Id$

-- End of file

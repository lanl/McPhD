-- TryNSave.hs
-- T. M. Kelley
-- Feb 28, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module TryNSave (writeTally)
    where

import Tally

writeTally :: Tally -> String -> IO ()
writeTally t name = do
  writeFile name (show t)


-- version
-- $Id$

-- End of file

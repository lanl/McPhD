-- MPISwitch.hs
-- T. M. Kelley
-- Apr 30, 2012
-- (c) Copyright 2012 LANSLLC, all rights reserved

module MPISwitch (toRank
                 ,Rank
                 ,sendTally
                 ,recvTally
                 ,commWorld
                 ,commRank
                 ,commSize
                 ,fromRank
                 ,wtime
                 ,init
                 ,finalize
                 ,diffTime
                 )
  where

import Prelude hiding (init)

#ifdef UseMPI
import MPIFull
#else
import MPIStub
#endif

-- End of file

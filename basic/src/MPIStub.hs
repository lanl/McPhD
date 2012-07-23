-- MPIStub.hs
-- T. M. Kelley
-- Mar 26, 2012
-- (c) Copyright 2012 LANSLLC, all rights reserved

-- | Stubs for MPI for serial runs
module MPIStub (sendTally
               ,recvTally
               ,commWorld
               ,commRank
               ,commSize
               ,wtime
               ,init
               ,finalize
               ,toRank
               ,Rank
               ,fromRank
               ,diffTime 
               )
  where

import Prelude hiding (init)

import Data.Word (Word32)
import Data.Time

data Rank = Rank { fromRank :: Word32} deriving (Show,Eq,Ord)

commWorld :: Word32
commWorld  = 1
commRank :: Word32 -> IO Rank
commRank _ = return (Rank 0)
commSize :: Word32 -> IO Word32
commSize _ = return 1

toRank :: Enum a => a -> Rank
toRank i = Rank (fromIntegral . fromEnum $ i)

init, finalize :: IO ()
init     = return ()
finalize = return ()

sendTally _ = return ()
recvTally t = return t

wtime :: IO UTCTime 
wtime = getCurrentTime

diffTime :: UTCTime -> UTCTime -> NominalDiffTime
diffTime = diffUTCTime

-- End of file

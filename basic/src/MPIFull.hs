-- MPIFull.hs
-- T. M. Kelley
-- Apr 30, 2012
-- (c) Copyright 2012 LANSLLC, all rights reserved

module MPIFull (toRank
               ,Rank(..)
               ,sendTally
               ,recvTally
               ,commWorld
               ,commRank
               ,commSize
               ,fromRank
               ,wtime
               ,MPI.init
               ,finalize
               ,diffTime
               )
  where

import Control.Parallel.MPI.Simple as MPI
import Data.Serialize as Serialize
import qualified Data.List as L

sendTally :: Serialize a => a -> IO ()
sendTally t = MPI.gatherSend commWorld 0 (Serialize.encode t)

recvTally :: (Serialize a,Monoid a) => a -> IO a
recvTally t = do
  msgs <- MPI.gatherRecv commWorld 0 (Serialize.encode t)
  let ts = map (forceEither . Serialize.decode) msgs
  return $ L.foldl' mappend mempty ts

-- Cribbed from MissingH
forceEither :: Show e => Either e a -> a
forceEither (Left x) = error (show x)
forceEither (Right x) = x

diffTime :: Double -> Double -> Double
diffTime = (-)

-- End of file

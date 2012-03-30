-- | This library implements a pure interface to the Lagged (Additive) Fibonacci Generator,
--
module System.Random.LFG.Pure
   ( sequences
   , oneSequence
   ) where

import System.Random.LFG as LFG
import Control.Monad.ST.Lazy as L (ST, runST, strictToLazyST)
import Data.Word (Word32)

-- | Run a generator to produce a list of elements lazily.
genRands :: GenST s -> L.ST s [Word32]
genRands gen = do
   next <- strictToLazyST $ uniform gen
   ys <- genRands gen
   return (next : ys)

-- | Given a finite list of initial random values, rturn a ST computation yielding a
-- infinite sequence of random numbers. NOTE: the input sequence of
-- random numbers must have length at least K (the size of the lag table).
lfgST :: Lags -> [Word32] -> L.ST s [Word32]
lfgST lags is = do
   -- initialise the generator
   (gen:_) <- strictToLazyST $ LFG.create lags 1 is
   -- start producing the sequence
   genRands gen

oneSequence :: Lags -> [Word32] -> [Word32]
oneSequence lags is = L.runST $ lfgST lags is

sequences :: Lags -> [Word32] -> [[Word32]]
sequences lags is = map (oneSequence lags) $ chunks (largeLag lags) is



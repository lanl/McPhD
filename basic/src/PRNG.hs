{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PRNG where

import Control.Monad.State.Strict
import Control.Monad.Identity
import System.Random as R

newtype RNG = RNG StdGen

mkRNG :: Int -> RNG
mkRNG = RNG . mkStdGen

testRNG :: RNG
testRNG = mkRNG 42

-- QUESTION: Should we make a Random monad?

newtype Rnd a = Rnd (State RNG a)
  deriving (Monad, MonadState RNG)

random :: Random a => Rnd a
random = Rnd (StateT (\ (RNG g) ->
                      case R.random g of (x, ng) -> Identity (x, RNG ng)))

randoms :: Random a => Int -> Rnd [a]
randoms n = replicateM n PRNG.random

runRnd :: RNG -> Rnd a -> (a, RNG)
runRnd g (Rnd m) = runState m g

split :: RNG -> (RNG, RNG)
split (RNG g) = case R.split g of
                  (g1, g2) -> (RNG g1, RNG g2)

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PRNG where

import Control.Monad.State.Strict
import Control.Monad.Identity
import System.Random as R
import Test.QuickCheck

newtype RNG = RNG StdGen deriving Show

mkRNG :: Int -> RNG
mkRNG = RNG . mkStdGen

testRNG :: RNG
testRNG = mkRNG 42

-- QUESTION: Should we make a Random monad?

newtype Rnd a = Rnd (State RNG a)
  deriving (Monad, MonadState RNG)

-- We can make a Rnd-value into a testable value.
-- In the 'property' method, we specify how to initialize
-- the random number generator in this case. Currently, we
-- take a fixed RNG here! But we could and probably should
-- initialize in a different way.
instance Testable a => Testable (Rnd a) where
  property s = property (fst (runRnd testRNG s))

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

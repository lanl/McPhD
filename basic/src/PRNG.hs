{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, FlexibleInstances #-}
module PRNG where

import Control.Monad.State.Strict
import Control.Monad.Identity
import System.Random as R
import Test.QuickCheck
import Numerical (FP)

-- convenient aliases
type RNG = XRNG FP
type Rnd = XRnd FP

-- a standard psuedo-random number generator
newtype PRNG = PRNG StdGen deriving Show

-- a type to provide values from a list
newtype FakeRNG s = FakeRNG { testrng :: [s]} deriving Show

mkRNG :: Int -> RNG
mkRNG = RNGp . PRNG . mkStdGen

testRNG :: RNG
testRNG = mkRNG 42

data XRNG s = RNGp { prng :: PRNG }
            | RNGt { trng :: FakeRNG s } deriving Show

-- | a monad that handles RNG state for different underlying RNG types
newtype XRnd s a = XRnd (State (XRNG s) a)
  deriving (Monad, MonadState (XRNG s))

-- | draw the next random number from the stream
{-# INLINE random #-}
random ::  Random a => XRnd a a
random = XRnd (StateT rndf)

rndf :: Random a => XRNG a -> Identity (a,XRNG a)
rndf (RNGt tg)       = advance tg
rndf (RNGp (PRNG g)) = 
  case R.random g of (x, ng) -> Identity (x, RNGp $ PRNG ng)

advance :: FakeRNG a -> Identity (a, XRNG a)
advance (FakeRNG (x:xs)) = Identity (x, RNGt $ FakeRNG xs)
advance (FakeRNG [])     = error "advance FakeRNG: Exhausted list of values"

randoms :: Random a => Int -> XRnd a [a]
randoms n = replicateM n PRNG.random

runRnd :: XRNG s -> XRnd s a -> (a, XRNG s)
runRnd g (XRnd m) = runState m g

split :: XRNG s -> (XRNG s, XRNG s)
split (RNGp (PRNG g))     = case R.split g of
                              (g1, g2) -> (RNGp $ PRNG g1, RNGp $ PRNG g2)
split (RNGt (FakeRNG xs)) = (RNGt $ FakeRNG xs, RNGt $ FakeRNG xs)

-- We can make a Rnd-value into a testable value.
-- In the 'property' method, we specify how to initialize
-- the random number generator in this case. Currently, we
-- take a fixed RNG here! But we could and probably should
-- initialize in a different way.

instance Testable a => Testable (XRnd FP a) where
  property s = property (fst (runRnd testRNG s))

-- end of file


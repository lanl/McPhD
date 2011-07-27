{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Presents an abstract type random number generation and various
-- functions for working with it.

module RandomNumbers ( RNG() -- An abstract type for the random number generator.
                     , Seed(..)
                     , makeRNG
                     , randomDouble
                     , random
                     , randoms
                     , sampleN
                     , samples
                       )where

import Control.Monad.State.Strict
import Control.Monad.Identity

import qualified System.Random as R
import qualified System.Random.Mersenne.Pure64 as Pure
import Data.List

import Numerics

-- * Defining an abstract random number generator: RNG
-- XXX: Why not make RNG an instance of RandomGen?                                  

newtype Seed = Seed { toInt :: Integer } deriving Show
newtype RNG = RNG Pure.PureMT deriving Show

makeRNG :: Seed -> RNG
makeRNG = RNG . Pure.pureMT . fromIntegral . toInt

-- | A function which directly accesses the fast randomDouble method
-- of the Pure64 rng. 
randomDouble :: RNG -> (Double, RNG)
randomDouble (RNG rand) = (d, RNG rand') where (d,rand') = Pure.randomDouble rand

split :: RNG -> (RNG, RNG)
split (RNG g) = case R.split g of (g1,g2) -> (RNG g1, RNG g2)
                                  

-- | A State Monad which propagates the RNG random state through a computation
newtype Rnd a = Rnd (State RNG a) deriving (Monad, MonadState RNG)

-- Create a random value of type a, wrapped in the Rnd monad, i.e.
-- the result is a function RNG -> (RNG, a)

-- XXX Why use the transformer?  Could we write this directly in terms
-- of State?
random :: R.Random a => Rnd a
random = Rnd (StateT (\(RNG g) -> case R.random g of (x,ng) -> Identity (x, RNG ng)))

randoms :: R.Random a => Int -> Rnd [a]
randoms n = replicateM n random

-- Deconstruct Rnd and pass it on to runState. The argument order is reversed wrt runState.
runRnd :: RNG      -- ^ An initial random state
       -> Rnd a    -- ^ A computation RNG -> (a,RNG) to execute.
       -> (a,RNG)  -- ^ Result of the computation and the new random state.
runRnd g (Rnd m) = runState m g






-- | Get N samples from the given random function and RNG. Orginal non-monadic function.
sampleN :: (RNG -> (a, RNG)) -> RNG -> Int -> ([a], RNG)
sampleN generator rand n
  | n <= 0 = ([], rand)
  | otherwise =
      let (value, rand')   = generator rand
          (values, rand'') = sampleN generator rand' (n-1)
      in (value : values, rand'')




-- TODO: Depending on application, you might want to make the functions
-- here more strict, such that forcing the top-level of the result
-- automatically forces the entire list being returned. Generally, I'd
-- assume that you want random numbers computed as soon as possible,
-- to prevent them being evaluated on another CPU, and to prevent space
-- leaks. But I might be wrong, which is why I'm not yet making the
-- change.

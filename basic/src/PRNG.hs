{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts,
  RankNTypes,
  MultiParamTypeClasses,
  TypeSynonymInstances,
  BangPatterns,
  GADTs #-}
module PRNG where

import Control.Monad.State
import Test.QuickCheck hiding ((.&.))
import Numerical (FP)
import Data.Word (Word32)
import Data.Int (Int32)
import Data.Bits
import Data.List
import qualified Data.Vector.Unboxed as V

import System.Random as R
import System.Random.LFG (defaultLags, largeLag)
import System.Random.LFG.Pure as LFG
import qualified System.Random.MWC as MWC

-- | A pure generator is a lazy stream of words (bits).
data RNG = RNG !FP RNG

unfoldRNG :: (s -> (FP, s)) -> s -> RNG
unfoldRNG f = go
  where
    go !s = case f s of
              (!x, !y) -> RNG x (go y)

-- | The Rnd monad carries around the generator.
newtype Rnd a = Rnd { unRnd :: forall r. (a -> RNG -> r) -> RNG -> r }

instance Monad Rnd where
  return x = Rnd (\ k -> k x)
  Rnd m >>= f = Rnd (\ k -> m (\ x -> unRnd (f x) k))

instance MonadState RNG Rnd where
  get =   Rnd (\ k s -> k s s)
  put x = Rnd (\ k _ -> k () x)

instance Functor Rnd where
  fmap f (Rnd g) = Rnd (\ k -> g (k . f))

instance Applicative Rnd where
  f <*> v = undefined

runRnd :: RNG -> Rnd a -> a
runRnd g (Rnd x) = x const g

random :: Rnd FP
random = do
           RNG x g <- get
           put g
           return x
{-# INLINE random #-}

-- | From mwc-random. Converts two 32-bit words to a double
-- in the range [0, 1].
wordsToDouble :: Word32 -> Word32 -> Double
wordsToDouble x y  = (fromIntegral u * m_inv_32 + (0.5 + m_inv_53) +
                     fromIntegral (v .&. 0xFFFFF) * m_inv_52)
    where m_inv_52 = 2.220446049250313080847263336181640625e-16
          m_inv_53 = 1.1102230246251565404236316680908203125e-16
          m_inv_32 = 2.3283064365386962890625e-10
          u        = fromIntegral x :: Int32
          v        = fromIntegral y :: Int32
{-# INLINE wordsToDouble #-}

{-# NOINLINE createLFG #-}
createLFG :: Int -> Int -> IO [RNG]
createLFG s n =
  do
    mwc <- MWC.initialize (V.singleton (fromIntegral s))
    initials <- replicateM (n * largeLag defaultLags) (MWC.uniform mwc)
    let mkRNG = unfoldRNG (\ (x : y : zs) -> (wordsToDouble x y, zs))
    return $ map mkRNG $ LFG.sequences defaultLags initials

makeStdGen :: Int -> Int -> [RNG]
makeStdGen s n =
    let stdgen = R.mkStdGen s
        mkRNG  = unfoldRNG R.random
    in take n $ map mkRNG $ unfoldr (Just . split) stdgen

testRNG :: RNG
testRNG = head $ makeStdGen 42 1

{-# NOINLINE createStdGen #-}
createStdGen :: Int -> Int -> IO [RNG]
createStdGen s n = return $ makeStdGen s n

makeFakeGen :: [Double] -> RNG
makeFakeGen = unfoldRNG (\ (x : xs) -> (x, xs))

instance Testable a => Testable (Rnd a) where
  property = property . runRnd testRNG

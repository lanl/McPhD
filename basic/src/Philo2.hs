-- Philo2.hs
-- Dec. 26, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}

module Philo2 where

-- Implementation of the Philox counter-based random number generator,
-- as described by Salmon et al.,

-- import Data.Word (Word32,Word64)
-- import Data.Int  (Int32)
import Data.Word 
import Data.Int  
import Data.Bits
import Control.DeepSeq
import Numerical

data Philo4x32Ctr = Ctr4x32 {-# UNPACK #-} !Word32 !Word32 !Word32 !Word32 
                    deriving (Show)
data Philo4x32Key = Key4x32 {-# UNPACK #-} !Word32 !Word32 deriving (Show)

data RNG = RNG Philo4x32Ctr Philo4x32Key deriving (Show)

instance NFData Philo4x32Ctr
instance NFData Philo4x32Key
instance NFData RNG

newtype NRounds = NRounds Word32

newtype Seed    = Seed Word32

newtype Offset  = Offset Word32

mkCtrs :: Word32 -> [Philo4x32Ctr]
mkCtrs 0 = []
mkCtrs n = go 1 n
  where
    go _ 0 = []
    go i n = Ctr4x32 i 0 0 0 : go (i+1) (n - 1)

zeroCtr :: Philo4x32Ctr
zeroCtr = Ctr4x32 0 0 0 0

-- | generate a list of 2x32 keys, each with high 32b equal to the seed, and
-- low 32b equal to a sequential offset.
mkKeys :: Seed -> Word32 -> Offset -> [Philo4x32Key]
mkKeys _ 0 _ = []
mkKeys (Seed s) n (Offset o) = go o n
  where go _ 0 = []
        go i n = Key4x32 s i : go (i+1) (n-1)

mulHiLo :: Word32 -> Word32 -> (Word32,Word32)
mulHiLo !a !b = (a*b,fromIntegral $ hi64 `shiftR` 32)
  where hi64 :: Word64
        hi64 = fromIntegral a * fromIntegral b
{-# INLINE mulHiLo #-}

-- hard-coded constants
philo_W32_0, philo_W32_1 :: Word32
philo_W32_0 = 0x9E3779B9
philo_W32_1 = 0xBB67AE85

philo_M4x32_0, philo_M4x32_1 :: Word32
philo_M4x32_0 = 0xD2511F53
philo_M4x32_1 = 0xCD9E8D57

philo4x32Bump :: Philo4x32Key -> Philo4x32Key
philo4x32Bump (Key4x32 k1 k2) = Key4x32 (k1 + philo_W32_0) (k2 + philo_W32_1)
{-# INLINE philo4x32Bump #-}

philo4x32Round :: Philo4x32Ctr -> Philo4x32Key -> Philo4x32Ctr
philo4x32Round (Ctr4x32 c0 c1 c2 c3) (Key4x32 k0 k1) = 
  let (lo0,hi0) = mulHiLo philo_M4x32_0 c0
      (lo1,hi1) = mulHiLo philo_M4x32_1 c2
      in Ctr4x32 (hi1 `xor` c1 `xor` k0) lo1 (hi0 `xor` c3 `xor` k1) lo0
{-# INLINE philo4x32Round #-}

-- | 10 round philox 4x32
philo4x32 :: Philo4x32Ctr -> Philo4x32Key -> Philo4x32Ctr
philo4x32 ctr key = go ctr key 10
  where go :: Philo4x32Ctr -> Philo4x32Key -> Int -> Philo4x32Ctr
        go !c !k 1 = philo4x32Round c k
        go !c !k i = let c1 = philo4x32Round c k
                         k1 = philo4x32Bump k
                     in go c1 k1 (i-1)
{-# INLINE philo4x32 #-}

wordsToDouble :: Word32 -> Word32 -> Double
wordsToDouble x y  = (fromIntegral u * m_inv_32 + (0.5 + m_inv_53) +
                     fromIntegral (v .&. 0xFFFFF) * m_inv_52)
    where m_inv_52 = 2.220446049250313080847263336181640625e-16
          m_inv_53 = 1.1102230246251565404236316680908203125e-16
          m_inv_32 = 2.3283064365386962890625e-10
          u        = fromIntegral x :: Int32
          v        = fromIntegral y :: Int32
{-# INLINE wordsToDouble #-}

-- | Get two doubles from a single RNG state
random2 :: RNG -> (URD,URD)
random2 (RNG c k) =
  let Ctr4x32 c1 c2 c3 c4 = philo4x32 c k
  in (URD $ wordsToDouble c1 c2, URD $ wordsToDouble c3 c4)
{-# INLINE random2 #-}

get :: RNG -> Philo4x32Ctr
get (RNG c k) = philo4x32 c k

incrRNG :: RNG -> RNG
incrRNG (RNG c k) = RNG (incrCtr1 c (Offset 1)) k

advanceRNG :: RNG -> Offset -> RNG
advanceRNG (RNG c k) n = RNG (incrCtr1 c n) k

-- | increment a counter by 1 in the least significant word. 
-- This should be sufficient to get another 128b of output 
-- goodness. This will not carry correctly. 
incrCtr1 :: Philo4x32Ctr -> Offset -> Philo4x32Ctr
incrCtr1 (Ctr4x32 c1 c2 c3 c4) (Offset n) = Ctr4x32 c1 c2 c3 (c4+n)

-- | increment a 4x32 counter in its most significant word. For subdividing
-- a stream.
incrCtr4 :: Philo4x32Ctr -> Offset -> Philo4x32Ctr
incrCtr4 (Ctr4x32 c1 c2 c3 c4) (Offset n) = Ctr4x32 (c1+n) c2 c3 c4

-- | increment a 4x32 counter in its second most significant word. For
-- subdividing a stream. 
incrCtr3 :: Philo4x32Ctr -> Offset -> Philo4x32Ctr
incrCtr3 (Ctr4x32 c1 c2 c3 c4) (Offset n) = Ctr4x32 c1 (c2+n) c3 c4

-- | increment a 4x32 counter in its second least significant word. For
-- subdividing a stream. 
incrCtr2 :: Philo4x32Ctr -> Offset -> Philo4x32Ctr
incrCtr2 (Ctr4x32 c1 c2 c3 c4) (Offset n) = Ctr4x32 c1 c2 (c3+n) c4


{- 

-- Ooh this is a really ugly hack: really just threw this in to compare 
-- with C++ version (in the NuT code).
randoms :: RNG -> Int -> (RNG,[URD])
randoms g1 i = go g1 0
  where go g n | n < i  = let g2 = incrRNG g 
                              (u1,u2) = random2 g
                              (ginf,us) = go g2 (n + 2)
                          in (ginf,u1:u2:us)
        go g n | n >= i = let g2 = incrRNG g
                              (u1,u2) = random2 g
                          in (g2,u1:u2:[])
 
-}

-- version
-- $Id$

-- End of file

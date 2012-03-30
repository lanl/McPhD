{-# LANGUAGE DeriveDataTypeable, CPP, TypeFamilies, FlexibleContexts  #-}

-- | This library implements a Lagged (Additive) Fibonacci Generator.

module System.Random.LFG
   (
     Variate (..)
   , Gen
   , GenIO
   , GenST
   , Lags (..)
   -- , Lags (smallLag, largeLag)
   , defaultLags
   , create
     -- * utilities
   , chunks
   ) where


import Control.Monad (ap, liftM, when)
import Data.Typeable (Typeable)
import Control.Exception (Exception, throw)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import qualified Data.Vector.Unboxed.Mutable as M
   (unsafeRead, unsafeWrite, unsafeNew, MVector, Unbox)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (ST)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Bits (Bits, (.&.), (.|.), shiftL)

-- | A generator of an infinite (but periodic) sequence of pseudo random numbers.
data Gen s =
   Gen { gen_table :: M.MVector s Word32
       , gen_j :: Int
       , gen_k :: Int
       }
-- the size of the vector is K+1 elements. Elements at indices [0,K-1]
-- constitute the values of the lagTable. The element at index K is special,
-- it contains the value of the current cursor into the table.

-- | A shorter name for PRNG state in the IO monad.
type GenIO = Gen (PrimState IO)

-- | A shorter name for PRNG state in the ST monad.
type GenST s = Gen (PrimState (ST s))

-- | Abstract type representing the lags of the generator.
data Lags = Lags { smallLag :: Int, largeLag :: Int } deriving Show

-- | default lag values: these must be carefully chosen
defaultLags :: Lags
defaultLags = Lags 861 1279

-- | An exception type to indicate that the generator was not initialised correctly,
-- namely that the value of J was >= K.
data LFGException = InitLagException String
   deriving (Show, Typeable)

instance Exception LFGException

-- | Create a list of psuedo random number generators, given some initial lag values, the
-- number of generators needed, and an initial sequence of random numbers.
create :: PrimMonad m
           => Lags      -- ^ lag values (j, k), j < k
           -> Int       -- ^ number of generators (n)
           -> [Word32]  -- ^ initial values (length must be >= n * k)
           -> m [Gen (PrimState m)]
create lags n is =
   mapM (createOne lags) $ take n $ chunks (largeLag lags) is

-- | Initialise a LFG using the small lag, large lag and a finite list of
-- seed elements (these should be "random"), ie use another random
-- number generator to make them. The input list of random numbers must
-- have length >= K, the size of the lag table.
createOne :: PrimMonad m => Lags -> [Word32] -> m (Gen (PrimState m))
createOne lags is = do
   let j = smallLag lags
       k = largeLag lags
   -- check that the lags are appropriate
   when (j >= k) $ throw $ InitLagException "small lag was >= large lag"
   -- check that enough initial values have been provided
   when (length is < k) $ throw insufficientInitials
   -- initialise a new lag table from the input elements
   vector <- M.unsafeNew (k + 1)
   -- copy initial elements into the table
   fill vector is k
   -- set the cursor to index 1 (the cursor lies at position k in the table)
   M.unsafeWrite vector k 1
   -- return the initial state of the generator
   return $ Gen { gen_table = vector
                , gen_j = j
                , gen_k = k
                }
   where
   -- copy elements from list to vector
   fill :: PrimMonad m => M.MVector (PrimState m) Word32 -> [Word32] -> Int -> m ()
   fill _vector [] n
      | n == 0 = return ()
      -- we ran out of inital values before filling the vector
      -- this will be avoided by a check in create, but it doesn't
      -- hurt to make it explicit here.
      | otherwise = throw insufficientInitials
   fill vector (x:xs) n
      | n == 0 = return ()
      | otherwise = do
           let n' = n - 1
           M.unsafeWrite vector n' x
           fill vector xs n'
   insufficientInitials =
      InitLagException "insufficient number of initial values (less than large lag)"

-- | Advance the generator by one step, yielding the next value in the sequence.
{-# INLINE uniformWord32 #-}
uniformWord32 :: PrimMonad m => Gen (PrimState m) -> m Word32
uniformWord32 (Gen { gen_table = table, gen_j = j, gen_k = k }) = do
   -- read the value of the cursor.
   currentIndexWord <- M.unsafeRead table k
   -- convert the cursor to Int, the vector type has Int indices, but our table stores Word32s.
   let currentIndex = fromIntegral currentIndexWord
   -- locate the jth and kth values in the table as offsets from the cursor.
   jth <- M.unsafeRead table (wrapIndex (currentIndex + j))
   kth <- M.unsafeRead table (wrapIndex (currentIndex + k))
   -- compute the next element from: x_i = (x_j + x_k) mod (2^32)
   -- the natural modular arithmetic of Word32 will wrap around.
   let nextElement = jth + kth
   -- overwrite the value at the cursor with the new element.
   M.unsafeWrite table currentIndex nextElement
   -- increment the cursor, and wrap if necessary.
   M.unsafeWrite table k $ fromIntegral $ nextIndex currentIndex
   -- return the next element in the sequence.
   return nextElement
   where
   -- increment an index and wrap to zero if we reach the end of the table.
   nextIndex :: Int -> Int
   nextIndex i = if i == k - 1 then 0 else i + 1
   -- wrap an index around the end of the table, much faster than using mod.
   wrapIndex :: Int -> Int
   wrapIndex index
      | index >= k = index - k
      | otherwise = index

-- | Split a list into chunks lazily, will be _|_ if the list is insufficiently long.
chunks :: Int -> [a] -> [[a]]
chunks size xs
   = case splitAt size xs of
        (c,rest) -> c : chunks size rest

-- From here on down, the rest of the code is taken directly from the random-mwc
-- library. Thanks to Bryan O'Sullivan for writing the code. The code is
-- copyright 2009, 2010, 2011 to Bryan O'Sullivan, and released under the BSD3
-- license.

-- | Yield a random value, by transforming just one Word32 value.
uniform1 :: PrimMonad m => (Word32 -> a) -> Gen (PrimState m) -> m a
uniform1 f gen = do
  i <- uniformWord32 gen
  return $! f i
{-# INLINE uniform1 #-}

-- | Yield a random value, by transforming two sequential Word32 values.
uniform2 :: PrimMonad m => (Word32 -> Word32 -> a) -> Gen (PrimState m) -> m a
uniform2 f gen = do
   w1 <- uniformWord32 gen
   w2 <- uniformWord32 gen
   return $! f w1 w2
{-# INLINE uniform2 #-}

-- | The class of types for which we can generate uniformly
-- distributed random variates.
class M.Unbox a => Variate a where
    -- | Generate a single uniformly distributed random variate.  The
    -- range of values produced varies by type:
    --
    -- * For fixed-width integral types, the type's entire range is
    --   used.
    --
    -- * For floating point numbers, the range (0,1] is used. Zero is
    --   explicitly excluded, to allow variates to be used in
    --   statistical calculations that require non-zero values
    --   (e.g. uses of the 'log' function).
    --
    -- To generate a 'Float' variate with a range of [0,1), subtract
    -- 2**(-33).  To do the same with 'Double' variates, subtract
    -- 2**(-53).
    uniform :: (PrimMonad m) => Gen (PrimState m) -> m a
    -- | Generate single uniformly distributed random variable in a
    -- given range.
    --
    -- * For integral types inclusive range is used.
    --
    -- * For floating point numbers range (a,b] is used if one ignores
    --   rounding errors.
    uniformR :: (PrimMonad m) => (a,a) -> Gen (PrimState m) -> m a

instance Variate Int8 where
    uniform = uniform1 fromIntegral
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Int16 where
    uniform = uniform1 fromIntegral
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Int32 where
    uniform = uniform1 fromIntegral
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Int64 where
    uniform = uniform2 wordsTo64Bit
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Word8 where
    uniform = uniform1 fromIntegral
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Word16 where
    uniform = uniform1 fromIntegral
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Word32 where
    uniform = uniform1 fromIntegral
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Word64 where
    uniform = uniform2 wordsTo64Bit
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Bool where
    uniform = uniform1 wordToBool
    uniformR (False,True)  g = uniform g
    uniformR (False,False) _ = return False
    uniformR (True,True)   _ = return True
    uniformR (True,False)  g = uniform g
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Float where
    uniform = uniform1 wordToFloat
    uniformR (x1,x2) = uniform1 (\w -> x1 + (x2-x1) * wordToFloat w)
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Double where
    uniform = uniform2 wordsToDouble
    uniformR (x1,x2) = uniform2 (\w1 w2 -> x1 + (x2-x1) * wordsToDouble w1 w2)
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Int where
#if WORD_SIZE_IN_BITS < 64
    uniform = uniform1 fromIntegral
#else
    uniform = uniform2 wordsTo64Bit
#endif
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Word where
#if WORD_SIZE_IN_BITS < 64
    uniform = uniform1 fromIntegral
#else
    uniform = uniform2 wordsTo64Bit
#endif
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance (Variate a, Variate b) => Variate (a,b) where
    uniform g = (,) `liftM` uniform g `ap` uniform g
    uniformR ((x1,y1),(x2,y2)) g = (,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

instance (Variate a, Variate b, Variate c) => Variate (a,b,c) where
    uniform g = (,,) `liftM` uniform g `ap` uniform g `ap` uniform g
    uniformR ((x1,y1,z1),(x2,y2,z2)) g =
      (,,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g `ap` uniformR (z1,z2) g
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

instance (Variate a, Variate b, Variate c, Variate d) => Variate (a,b,c,d) where
    uniform g = (,,,) `liftM` uniform g `ap` uniform g `ap` uniform g
                `ap` uniform g
    uniformR ((x1,y1,z1,t1),(x2,y2,z2,t2)) g =
      (,,,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g `ap`
                    uniformR (z1,z2) g `ap` uniformR (t1,t2) g
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

wordsTo64Bit :: (Integral a) => Word32 -> Word32 -> a
wordsTo64Bit x y =
    fromIntegral ((fromIntegral x `shiftL` 32) .|. fromIntegral y :: Word64)
{-# INLINE wordsTo64Bit #-}

wordToBool :: Word32 -> Bool
wordToBool i = (i .&. 1) /= 0
{-# INLINE wordToBool #-}

wordToFloat :: Word32 -> Float
wordToFloat x      = (fromIntegral i * m_inv_32) + 0.5 + m_inv_33
    where m_inv_33 = 1.16415321826934814453125e-10
          m_inv_32 = 2.3283064365386962890625e-10
          i        = fromIntegral x :: Int32
{-# INLINE wordToFloat #-}

wordsToDouble :: Word32 -> Word32 -> Double
wordsToDouble x y  = (fromIntegral u * m_inv_32 + (0.5 + m_inv_53) +
                     fromIntegral (v .&. 0xFFFFF) * m_inv_52)
    where m_inv_52 = 2.220446049250313080847263336181640625e-16
          m_inv_53 = 1.1102230246251565404236316680908203125e-16
          m_inv_32 = 2.3283064365386962890625e-10
          u        = fromIntegral x :: Int32
          v        = fromIntegral y :: Int32
{-# INLINE wordsToDouble #-}

-- Type family for fixed size integrals. For signed data types it's
-- its unsigned couterpart with same size and for unsigned data types
-- it's same type
type family Unsigned a :: *

type instance Unsigned Int8  = Word8
type instance Unsigned Int16 = Word16
type instance Unsigned Int32 = Word32
type instance Unsigned Int64 = Word64
type instance Unsigned Int   = Word

type instance Unsigned Word8  = Word8
type instance Unsigned Word16 = Word16
type instance Unsigned Word32 = Word32
type instance Unsigned Word64 = Word64
type instance Unsigned Word   = Word

-- Subtract two numbers under assumption that x>=y and store result in
-- unsigned data type of same size
sub :: (Integral a, Integral (Unsigned a)) => a -> a -> Unsigned a
sub x y = fromIntegral x - fromIntegral y

add :: (Integral a, Integral (Unsigned a)) => a -> Unsigned a -> a
add m x = m + fromIntegral x

-- Generate uniform value in the range [0,n). Values must be
-- unsigned. Second parameter is random number generator
unsignedRange :: (PrimMonad m, Integral a, Bounded a) => a -> m a -> m a
unsignedRange n rnd = go
  where
    buckets = maxBound `div` n
    maxN    = buckets * n
    go = do x <- rnd
            if x < maxN then return (x `div` buckets)
                        else go
{-# INLINE unsignedRange #-}

-- Generate unformly distributed value in inclusive range.
uniformRange :: ( PrimMonad m
                , Integral a, Bounded a, Variate a
                , Integral (Unsigned a), Bounded (Unsigned a), Variate (Unsigned a))
             => (a,a) -> Gen (PrimState m) -> m a
uniformRange (x1,x2) g
  | x1 == minBound && x2 == maxBound = uniform g
  | otherwise                        = do x <- unsignedRange (sub x2 x1 + 1) (uniform g)
                                          return $! add x1 x
{-# INLINE uniformRange #-}

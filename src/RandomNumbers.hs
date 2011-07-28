-- | Contains utitlities for creating random samples.
module RandomNumbers where

import System.Random.Mersenne.Pure64
import Data.List

newtype Seed = Seed { toInt :: Integer } deriving (Show)
makePureMT :: Seed -> PureMT
makePureMT = pureMT . fromIntegral . toInt

-- | Get N samples from the given random function and PureMT
sampleN :: (PureMT -> (a, PureMT)) -> PureMT -> Int -> ([a], PureMT)
sampleN generator rand n
  | n <= 0 = ([], rand)
  | otherwise =
      let (value, rand') = generator rand
          (values, rand'') = sampleN generator rand' (n-1)
      in (value : values, rand'')

-- | This is dangerous, since it doesn't return the new random state
-- for use in subsequent calculations.
samples :: (PureMT -> (a, PureMT)) -> PureMT -> [a]
samples generator rand = unfoldr (Just . generator) rand

-- TODO: Depending on application, you might want to make the functions
-- here more strict, such that forcing the top-level of the result
-- automatically forces the entire list being returned. Generally, I'd
-- assume that you want random numbers computed as soon as possible,
-- to prevent them being evaluated on another CPU, and to prevent space
-- leaks. But I might be wrong, which is why I'm not yet making the
-- change.

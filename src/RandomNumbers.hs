module RandomNumbers where

import System.Random.Mersenne.Pure64
import Data.List

newtype Seed = Seed { toInt :: Integer }
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

-- | This looks dangerous, since it doesn't return the new random
-- state.
samples :: (PureMT -> (a, PureMT)) -> PureMT -> [a]
samples generator rand = unfoldr (Just . generator) rand




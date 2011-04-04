module RandomNumbers where

import System.Random.Mersenne.Pure64
import Data.List

newtype Seed = Seed { toInt :: Integer }
makePureMT :: Seed -> PureMT
makePureMT = pureMT . fromIntegral . toInt

-- | Get N samples from the given random function and PureMT
sampleN :: (PureMT -> (a, PureMT)) -> PureMT -> Int -> [a]
sampleN generator rand n
  | n <= 0 = []
  | otherwise =
    let (value, rand') = generator rand
    in value : ( sampleN generator rand' (n-1) )

samples :: (PureMT -> (a, PureMT)) -> PureMT -> [a]
samples generator rand = unfoldr (Just . generator) rand

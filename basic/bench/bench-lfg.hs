-- bench-lfg.hs
-- T. M. Kelley
-- Oct 17, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

import Control.Monad
import Criterion.Main
import PRNG

main :: IO ()
main =
  defaultMain [
    bench "LFG via runRnd random"    $ nfIO (rrun createLFG)
  , bench "StdGen via runRnd random" $ nfIO (rrun createStdGen)
  ]

rrun :: (Int -> Int -> IO [RNG]) -> IO [Double]
rrun gg =
  do
    (g:_) <- gg 42 1
    return $ runRnd g (replicateM 10000 PRNG.random)

-- version
-- $Id$

-- End of file

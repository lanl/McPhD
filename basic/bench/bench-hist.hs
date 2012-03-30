-- bench-hist.hs
-- Nov. 16, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

import Control.Monad
import Criterion.Main
import Histogram
import Physical

h :: EHist
h = mkHist (map Energy [1..1000])

main :: IO ()
main =
  defaultMain [
    bench "Histogram count"    $ nf (count h) (Energy 1.0, EnergyWeight 1.0)
  ]



-- version
-- $Id$

-- End of file

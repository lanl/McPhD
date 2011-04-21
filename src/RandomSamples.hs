module RandomSamples where

import System.Random.Mersenne.Pure64
import RandomNumbers
import Numerics
import NormalizedValues
import Data.Vector.V1

sampleVar :: PureMT -> (Var, PureMT)
sampleVar rand = (unsafe_makeUnitary d, rand')
    where (d,rand') = randomDouble rand

sampleNormalVector1 :: PureMT -> (Normalized Vector1, PureMT)
sampleNormalVector1 rand = (generateNormalVector1 xi, rand')
                           where (xi, rand') = sampleVar rand

sample_unit_sphere :: PureMT -> (Double, PureMT)
sample_unit_sphere rand =
    let (vars, rand') = sampleN randomDouble rand 3
    in (minimum vars, rand')
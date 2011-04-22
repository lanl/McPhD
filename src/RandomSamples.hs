{-- A module for creating random samples of various data types and numeric quantities.
--}
module RandomSamples where

import System.Random.Mersenne.Pure64
import RandomNumbers
import Numerics
import NormalizedValues
import Data.Vector.V1
import Data.Vector.V2
import Data.Vector.V3

-- The Var type is a uniform value in [0,1].
sampleVar :: PureMT -> (Var, PureMT)
sampleVar rand = (unsafe_makeUnitary d, rand')
    where (d,rand') = randomDouble rand

sampleNormalVector1 :: PureMT -> (Normalized Vector1, PureMT)
sampleNormalVector1 rand = (generateNormalVector1 xi, rand')
    where (xi, rand') = sampleVar rand


sampleNormalVector2 :: PureMT -> (Normalized Vector2, PureMT)
sampleNormalVector2 rand = (generateNormalVector2 xi, rand')
    where (xi, rand') = sampleVar rand
                                 
sampleNormalVector3 :: PureMT -> (Normalized Vector3, PureMT)
sampleNormalVector3 rand = (generateNormalVector3 xi1 xi2, rand'')
    where (xi1, rand') = sampleVar rand
          (xi2, rand'') = sampleVar rand'
          

sample_unit_sphere1D :: PureMT -> (Radius, PureMT)
sample_unit_sphere1D rand =
    let (vars, rand') = sampleN randomDouble rand 3
    in (Radius $ minimum vars, rand')
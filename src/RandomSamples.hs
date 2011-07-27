{-- A module for creating random samples of various data types and
numeric quantities.  I use "sample function" to mean something that
works with a RNG source of random numbers and returns a value and a
new RNG.

These functions often use generators, which are functions that convert
variants (uniformly distributed values in [0,1] into values.
--}
module RandomSamples where

import RandomNumbers
import Generators
import Numerics
import NormalizedValues
import Data.Vector.V1
import Data.Vector.V2
import Data.Vector.V3
import Test.QuickCheck.Modifiers

-- The Var type is a uniform value in [0,1].
sampleVar :: RNG -> (Var, RNG)
sampleVar rand = (unsafe_makeUnitary d, rand')
    where (d,rand') = randomDouble rand

sampleNormalVector1 :: RNG -> (Normalized Vector1, RNG)
sampleNormalVector1 rand = (generateNormalVector1 xi, rand')
    where (xi, rand') = sampleVar rand

sampleNormalVector2 :: RNG -> (Normalized Vector2, RNG)
sampleNormalVector2 rand = (generateNormalVector2 xi, rand')
    where (xi, rand') = sampleVar rand

sampleNormalVector3 :: RNG -> (Normalized Vector3, RNG)
sampleNormalVector3 rand = (generateNormalVector3 xi1 xi2, rand'')
    where (xi1, rand')  = sampleVar rand
          (xi2, rand'') = sampleVar rand'


-- | Sample a uniformly distributed point (with 1D symmetry) inside of
-- the ball of radius one.
sample_unit_ball1D :: RNG -> (Radius, RNG)
sample_unit_ball1D rand =
    let (vars, rand') = sampleN randomDouble rand 3
    in (Radius $ minimum vars, rand')

-- | Sample a uniformly distributed point (with 1D symmetry) inside of
-- ball of specified radius
sample_ball1D :: Radius -> RNG -> (Radius, RNG)
sample_ball1D (Radius radius) rand =
    let (Radius r_unit, rand') = sample_unit_ball1D rand
    in (Radius $ r_unit*radius, rand')

-- | Sample a uniformly distributed point (with 1D symmetry) inside of
-- a ball of given radius and outside the ball of another smaller
-- radius.
sample_annulus1D :: Radius -> Radius -> RNG -> (Radius, RNG)
sample_annulus1D (Radius r_min) (Radius r_max) rand =
    let width = r_max - r_min
        (Radius r, rand') = sample_ball1D (Radius width) rand
        r' = r + r_min
    in (Radius r', rand')


sampleInterval :: (Double, Double) -> RNG -> (Double, RNG)
sampleInterval (min, max) rand =
  let (var, rand') = sampleVar rand
  in (generateInterval (min, max) var, rand')


sampleExponential :: Double -> RNG -> (Double, RNG)
sampleExponential lambda rand =
  let (var, rand') = sampleVar rand
  in (generateExponential (Positive lambda) var, rand')

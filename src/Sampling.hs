module Sampling where

import Numerics
import Test.QuickCheck.Modifiers

sampleExponential :: Positive Double -> UnitInterval Double -> Double
sampleExponential (Positive lambda) (UnitInterval a) = -log (a)*lambda

{-- Generators are functions which map one or more uniform varaints
into a range of values, including ones represented by various data
types. These functons can also take additional values which are
parameters of the distribution.
--}
module Generators where

import Numerics
import Test.QuickCheck.Modifiers

generateExponential :: Positive Double -> Var -> Double
generateExponential (Positive lambda) (UnitInterval a) = -log (a)*lambda

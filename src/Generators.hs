module Generators where

import Numerics
import Test.QuickCheck.Modifiers

generateExponential :: Positive Double -> Var -> Double
generateExponential (Positive lambda) (UnitInterval a) = -log (a)*lambda


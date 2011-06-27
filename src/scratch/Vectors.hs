module Vectors where

import Data.Vector.Class
import Data.Vector.V2
import Control.Monad

newtype MyVector = MyVector { vec :: Vector2 }

instance BasicVector MyVector where
  vmap f  (MyVector v)                = MyVector (vmap f v)
  vzip f  (MyVector v1) (MyVector v2) = MyVector (vzip f v1 v2)
  vfold f (MyVector v)                = vfold f v
  vpack ss                            = liftM MyVector (vpack ss)
  vunpack (MyVector v)                = vunpack v
  vpromote s                          = MyVector (vpromote s)

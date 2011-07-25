{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}



module NumericClasses where

import Data.Vector.Class
import Data.Vector.V1
import Data.Vector.V2
import Data.Vector.V3

-- A class for things with a (-) operation.
class Diff r where 
    diff :: r -> r -> r
    
instance Num n => Diff n where diff = (-)

class Mag a where
  magnitude  :: a -> Double
  magnitude2 :: a -> Double  -- ^ Square of the magnitude. Often faster.
  magnitude2 x = (magnitude x) ^ (2 ::Integer)

class Scale v where
    scale :: v -> Double -> v

instance Mag Double   where magnitude = abs; magnitude2 = (^(2::Int))
instance Scale Double where scale = (*)

instance Mag Vector1   where magnitude = vmag; magnitude2 x = vdot x x
instance Scale Vector1 where scale = (|*)

instance Mag Vector2   where magnitude = vmag; magnitude2 x = vdot x x
instance Scale Vector2 where scale = (|*)

instance Mag Vector3   where magnitude = vmag; magnitude2 x = vdot x x
instance Scale Vector3 where scale = (|*)

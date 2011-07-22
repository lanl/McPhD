{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module NormalizedClass where

import Normalized

-- NormalClass is for types which are Normal (magnitude 1), but are
-- not necessairly a normalized version of something else.
class NormalClass n where    
  
instance (NormalClass n) => Mag n where
    magnitude  = const 1.0
    magnitude2 = const 1.0


-- NormalizedClass is for quantities which are normalized versions of something else.
class NormalizedClass n where
    type General n :: *
    denormal :: n -> General n
    
-- Everything which is a normalized foo, is normalized.
instance NormalizedClass n => NormalClass n where

-- The Normalized data type is in the Normalized Class.
instance NormalizedClass (Normalized n) where
    type General (Normalized n) = n
    denormal = getValue
    


  
  
-- Function on general normalized values
f2 :: (Fractional (General n), NormalizedClass n) => n -> General n
f2 x = 2.0 * denormal x


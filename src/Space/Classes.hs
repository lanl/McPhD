{-# LANGUAGE TypeFamilies, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}

module Space.Classes where

import Properties
import Approx ()

class Space s where
  type Position  s :: *
  type Direction s :: *
  stream    :: s -> Distance -> s
  position  :: s -> Position s
  direction :: s -> Direction s
  make      :: Position s -> Direction s -> s

-- | Infix operator for streaming.
infix 6 +->
(+->) :: (Space s) => s -> Distance -> s
(+->) = stream




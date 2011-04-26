{-# LANGUAGE TypeFamilies #-}

module SpaceTime.Classes where

import Approx ()


class Space s where
  type Position s  :: *
  type Direction s :: *
  type Distance s  :: *
  stream    :: s -> Distance s -> s
  position  :: s -> Position s
  direction :: s -> Direction s

(+->) :: (Space s) => s -> Distance s -> s
(+->) = stream

-- TODO: Do you want to add an infix declaration for (+->) ?

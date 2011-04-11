{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction #-}

module SpaceTime.Classes where

import Approx


class Space s where
  type Distance s :: *
  type Position s :: *
  type Direction s :: *
  stream    :: s -> Distance s -> s
  position  :: s -> Position s
  direction :: s -> Direction s
  

(+->) :: (Space s) => s -> Distance s -> s
(+->) = stream

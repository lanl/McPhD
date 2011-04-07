{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction #-}

module SpaceTime.Classes where

import Approx


class Space s where
  type Distance s :: *
  stream :: s -> Distance s -> s

(+->) :: (Space s) => s -> Distance s -> s
(+->) = stream

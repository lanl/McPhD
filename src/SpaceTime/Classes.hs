{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction #-}

module SpaceTime.Classes where

class Space s where
  type Distance s :: *
  stream :: s -> Distance s -> s

(+->) :: (Space s) => s -> Distance s -> s
(+->) = stream

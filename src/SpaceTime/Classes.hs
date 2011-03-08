{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction #-}

module SpaceTime.Classes where

class Space s where
  type Position  s :: *
  type Direction s :: *
  type Distance  s :: *
  stream :: Location s -> Distance s -> Location s

data Location s = Location { pos::Position s, dir::Direction s }
(+->) = stream

class (Space s) => SpaceTime s where
  type Time  s :: *
  type Speed s :: *
  timeFromDistance :: Speed s -> Distance s -> Time s
  distanceFromTime :: Speed s -> Time s -> Distance s

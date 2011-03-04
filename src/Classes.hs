module Classes where

class SpaceTime st where
  data Position :: *
  data Direction :: *
  type Location = (Position, Direction)
  data Momentum :: *
  data Motion :: *
  data Distance :: *
  data Direction :: *
  data Time :: *
  data Speed :: *
  motion ::    Direction -> Distance -> Motion
  move ::      Location -> Motion -> Location
  translate :: Location -> Distance -> Location

  


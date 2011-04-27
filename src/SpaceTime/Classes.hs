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

infix 6 +->
(+->) :: (Space s) => s -> Distance s -> s
(+->) = stream

-- TODO: Do you want to add an infix declaration for (+->) ?

-- ANS: I went with six, since the operation is conceptually similar
-- to addition. Not exactly a rigorous analysis. There aren't a lot of
-- operators it will appear with in expressions.
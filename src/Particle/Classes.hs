{-# LANGUAGE TypeFamilies #-}

{-| Generic interface to particles
-}
module Particle.Classes (
  Particle (..)
  , Tally (..)
  ) where


-- Typeclasses
-- -----------

class Particle p where
  type Environment p :: *  -- ^ Conditions govern. particle motion.
  type Event p :: *      -- ^ Results of step operations.
  -- | Step the particle
  step :: Environment p -> p -> Maybe (Event p, p)

class Tally t where
  data TallyContrib t :: *
  contribute :: t -> TallyContrib t -> t
{-# LANGUAGE TypeFamilies #-}

{-| Generic interface to particles
-}
module Particle.Classes where

import qualified Space3DCartesian as Space

-- * Particles

-- | A particle is something can can be stepped. It has associated
-- types which describe the context of the stepping and local
-- environment affecting each step. Events of another type are
-- produced with each step.
class Particle p where
  type ContextT p     :: * -- ^ Conditions govern. particle motion.
  type EnvironmentT p :: * -- ^ Local conditions relevant to each step.
  type EventT p       :: * -- ^ Results of step operations.

  -- | Get the current environment from the context and particle
  -- state.
  environment :: ContextT p -> p -> EnvironmentT p

  -- | Step the particle, producing an event and new particle.
  step :: EnvironmentT p -> p -> (EventT p, p)


-- | An InSpace particle also has methods for extracting it's space
-- related attributes and moving the particle.
class (Particle p) => InSpace p where
  position  :: p -> Space.Position
  direction :: p -> Space.Direction
  move      :: p -> Space.Distance -> p

-- | An InTime particle has an internal clock and a method for
-- advancing it.
class (Particle p) => InTime p where
  time :: p -> Space.Time
  tick :: p -> Space.Time -> p


-- | InSpaceTime particles have a speed, and can be advanced by
-- specifying either the time or distance of travel. These functions
-- have default values which use the appropiate methods from InSpace
-- and InTime.
class (InSpace p, InTime p) => InSpaceTime p where
  speed :: p -> Space.Speed

  advanceTime :: p -> Space.Time -> p
  advanceTime p t = tick (move p d) t where
      d = Space.distanceToTime t (speed p)

  advanceDistance :: p -> Space.Distance -> p
  advanceDistance p d = tick (move p d) t where
      t = Space.timeToDistance d (speed p)

-- | A RandomParticle has a random state associated with it. It can
-- return this state or generate a random sample and updated particle
class (Particle p) => RandomParticle p where
  type Random p :: *
  getRandom :: p -> Random p
  sample    :: p -> (Double, p)


-- * Events

-- | Events produce EventTally data, which is a contribution to a
-- global tally.
class Event e where
  type EventTally e :: *
  contribute :: e -> EventTally e
  is_final :: e -> Bool


-- * Tallies

-- | Tallies are collections of contributions into a global total.
class Tally t where
  type TallyEvent t :: *
  empty :: t
  combine :: TallyEvent t -> t -> t

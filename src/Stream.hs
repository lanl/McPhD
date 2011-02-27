{-# LANGUAGE TypeFamilies #-}

module Stream where

import Particle.Classes

stream :: (Particle p) => ContextT p -> p -> [EventT p]
stream context particle =
  let environment' = environment context
  in iterate next_step particle
     where next_step particle =
             let env = environment' particle


tally :: (Event e, Tally t) => t -> [e] -> t

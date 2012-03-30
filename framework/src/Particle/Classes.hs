{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Particle.Classes where

import Data.Ix
import Properties
import Space.Classes

class Particle p where
  move :: p -> Distance -> p

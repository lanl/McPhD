{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, UndecidableInstances, NoMonomorphismRestriction, StandaloneDeriving #-}
{-| A particle type which is paramaterized over the space it moves in.
-}
module Particle.IndexedParticle where

import Data.Function
import Data.Ix
import System.Random.Mersenne.Pure64

import Space.Classes
import Properties
import Approx

import Space.Test.Space_arbitrary ()


-- | Data type for particles with an index and a space.  This reduces
-- the coupling to details of the mesh. Including any other type
-- parameters that may be added to it.
data (Ix i, Space s) => IndexParticle i s = IndexParticle
    {
      piIndex    :: i
    , piLocation :: s
    , piTime     :: Time
    , piSpeed    :: Speed
    , piRand     :: PureMT
    } deriving Show

instance (Ix i, Space s, Approx s) => Approx (IndexParticle i s) where
    within_eps epsilon a b = (weps `on` piLocation) a b &&
                             (weps `on` piTime)     a b &&
                             ((==) `on` piIndex)    a b
        where weps = within_eps epsilon

{-# LANGUAGE DeriveGeneric #-}

module Material where

import Physical

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data Material = Material {
    mvel        :: !Velocity    -- cm/sec
  , tempE       :: !Temperature -- in energy units (MeV)
  , rhoNucl     :: !Density     -- g/cc
  , rhoEMinus   :: !NDensity    -- n/cc (derived from electron fraction)
  , rhoEPlus    :: !NDensity    -- n/cc
  } deriving (Show, Eq, Read, Generic)

instance NFData Material

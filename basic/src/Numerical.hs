{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Numerical where

-- We currently use 1-dimensional vectors.
type Vec = Double
type FP  = Double  -- floating-point value
newtype URD = URD Double  -- uniform random deviate [0,1]

type Idx = Int

newtype CellIdx = CellIdx { idx :: Idx }
  deriving (Eq, Show, Num)

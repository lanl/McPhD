{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Numerical where

-- We currently use 1-dimensional vectors.
type Vec = Double
type FP  = Double  -- floating-point value

type Idx = Int

newtype CellIdx = CellIdx { idx :: Idx }
  deriving (Eq, Show, Num)

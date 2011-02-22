{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A module containing properties of space
module Properties where

import Approx

newtype Opacity = Opacity { opValue :: Double } deriving (Eq, Show, Num, Ord, Approx)

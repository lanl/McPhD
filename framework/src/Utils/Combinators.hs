module Utils.Combinators where

import Control.Applicative
import Data.Monoid

-- | A variation on 'apply' which lifts the second argument.
(<*^>) :: (Applicative f) => f (a -> b) -> a -> f b
(<*^>) g a = g <*> (pure a)
infixl 4 <*^>

-- | Should and will be in Data.Monoid soon.
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
{-# INLINE (<>) #-}

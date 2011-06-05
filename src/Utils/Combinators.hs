module Utils.Combinators where

import Control.Applicative

-- | A variation on 'apply' which lifts the second argument.
(<*^>) :: (Applicative f) => f (a -> b) -> a -> f b
(<*^>) g a = g <*> (pure a)
infixl 4 <*^>

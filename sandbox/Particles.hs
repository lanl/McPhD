{-# OPTIONS_GHC -XMultiParamTypeClasses #-}

module Particles where

import Classes

data Simple = Simple { s_p :: Position,  s_d :: Direction } deriving Show
instance InSpace Simple where
  position   = s_p
  direction  = s_d
  
instance Streamable Simple Double where
  stream particle distance = Just $ (distance, Simple (move particle distance) (direction particle))

data LimitedParticle = LimitedParticle { l_p :: Position, l_d :: Direction, l_l :: Limit } deriving Show
instance InSpace LimitedParticle where
  position  = l_p
  direction = l_d
  
instance Limited LimitedParticle where
  value = l_l
  advance particle distance
    | distance < limit = (limit - distance, distance)
    | otherwise        = (0, limit)
    where limit = value particle

instance Streamable LimitedParticle Double where
  stream particle distance 
   | (value particle) <= 0 = Nothing
   | otherwise = Just (actual, LimitedParticle (move particle distance) (direction particle) remaining)
   where (remaining, actual) = advance particle distance



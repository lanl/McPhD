{-# OPTIONS_GHC -XMultiParamTypeClasses #-}

module Mesh.Classes (Mesh (..)) where


class Mesh m point direction distance fields cell face where
  cell     :: m -> point -> cell
  next     :: m -> face -> cell
  get      :: m -> cell -> fields
  distance :: m -> cell -> point -> direction -> (distance, face)
  
  
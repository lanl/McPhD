-- Stream_Test.hs
-- T. M. Kelley
-- Aug 11, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Test.Stream_Test (tests) where

-- The library under test
import Sphere1D
import MC

-- Test libraries
import Test.Framework (testGroup,Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Arbitraries ()

-- * properties of stream

-- | stream changes only position, direction, and time
eUnchanged :: Sphere1D -> EventCandidate ->  Particle -> Bool
eUnchanged msh ec p@(Particle { energy  = nrgi
                              , weight  = wti
                              , cellIdx = cidxi })
  = let (Particle { energy  = nrgo
                  , weight  = wto
                  , cellIdx = cidxo }) = stream msh p ec
    in nrgi == nrgo  &&  wti == wto  &&  cidxo == cidxi

-- | if stream has non-zero distance, then particle's position,
-- direction, and time must change (for Sphere1D mesh, anyway)
covary :: Sphere1D -> EventCandidate -> Particle -> Bool
covary msh ec p@(Particle { pos  = r
                          , dir  = o
                          , time = t})
  = let (Particle {pos = r', dir = o', time = t'}) = stream msh p ec
        d = candDist ec
    in if d /= 0
       then  r /= r'  &&  o /= o'  &&  t /= t'
       else  r == r'  &&  o == o'  &&  t == t'

tests :: [Test]
tests = [testGroup "stream"
         [
           testProperty "stream changes only position, direction, time" eUnchanged
         , testProperty "stream changes position, direction, time together" covary
         ]
        ]


-- version
-- $Id$

-- End of file

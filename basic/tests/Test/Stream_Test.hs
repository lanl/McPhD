-- Stream_Test.hs
-- T. M. Kelley
-- Aug 11, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Test.Stream_Test (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck
import Test.Arbitraries

-- The library under test
import Sphere1D
import Mesh
import Cell
-- import Physical
import PRNG
import Material
import SoftEquiv
import MC
import Sigma_HBFC
import Particle as P
-- import Constants
import Source

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

tests = [testGroup "stream"
         [ 
           testProperty "stream changes only position, direction, time" eUnchanged
         , testProperty "stream changes position, direction, time together" covary
         ]
        ]


-- version
-- $Id$

-- End of file

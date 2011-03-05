-- Source.hs
-- T. M. Kelley
-- Feb 15, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Source (genParticles
              ,genParticle)
    where

import Particle
import PRNG
import Physical
import Mesh
import System.Random (split)

-- generates particles uniformly over a spatial domain
genParticles :: Word32 ->   -- how many particles to generate
                Mesh ->    
                RNG ->
               [Particle]
genParticles 0 _ _ = []
genParticles n msh rng = 
  let (ps1,ps2,ps3,ds1,ds2,ds3,rng') = getSixRNs rng 
      (g1,g2) = split rng'
      (x,c)   = samplePosition msh ps1 ps2 ps3
      d       = sampleDirection msh ds1 ds2 ds3
      p       = Particle x d t e ew c g1 tag
  in p:genParticles (n-1) msh g2
  where t   = Time 1.0
        e   = Energy 1.0
        ew  = EnergyWeight 1.0
        tag = n::Word32

-- generates one particle uniformly over a spatial domain
genParticle :: Word32 ->   -- particle tag
                Mesh ->    
                RNG ->
                (Particle,RNG)
genParticle n msh rng = 
  let (ps1,ps2,ps3,ds1,ds2,ds3,rng') = getSixRNs rng 
      (g1,g2) = split rng'
      (x,c)   = samplePosition msh ps1 ps2 ps3
      d       = sampleDirection msh ds1 ds2 ds3
      p       = Particle x d t e ew c g1 tag
  in (p,g2)
  where t   = Time 1.0
        e   = Energy 1.0
        ew  = EnergyWeight 1.0
        tag = n::Word32

-- version
-- $Id$

-- End of file

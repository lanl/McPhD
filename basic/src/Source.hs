-- Source.hs
-- T. M. Kelley
-- Feb 15, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Source 
    where

import Particle
import Physical
import Mesh
import Control.Applicative ((<$>))

-- generates particles uniformly over a spatial domain
genParticles :: Word32 ->   -- how many particles to generate
                Mesh ->    
                RNG ->
               IO [Particle]
genParticles 0 msh rng = return []
genParticles n msh rng = do
  (x,c) <- samplePosition msh rng
  d <- sampleDirection msh rng
  let p = Particle x d t e ew c rng tag
  (p:) <$> (genParticles (n-1) msh rng)
  where -- d = Direction 1.0
        t = Time 1.0
        e = Energy 1.0
        ew = EnergyWeight 1.0
        tag = (n - 1)::Word32


-- version
-- $Id$

-- End of file

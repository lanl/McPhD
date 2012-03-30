module Constants where

import Numerical

-- | Speed of light in vacuum.
c :: FP
c = 2.99792458e10 -- cm/sec

-- | proton mass in g: needed to convert mass densities to number densities.
pmg :: FP  
pmg = 1.67262e-24

-- | Lower limit for event selection.
tiny :: FP
tiny = 1e-300

-- | Upper limit for event selection.
huge :: FP
huge = 1e+300

-- | Boltzmann constant, MeV/K
k_B :: Double
k_B = 8.61734e-11

-- | Convert erg to MeV
mevPerErg :: FP
mevPerErg = 6.24150974e5

-- | Convert MeV to erg
ergPerMeV :: FP
ergPerMeV = 1.60217646e-6

-- eof


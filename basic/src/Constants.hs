module Constants where

import Numerical

-- | Speed of light in vacuum.
c :: FP
c = 2.9979e10 -- cm/sec

-- | proton mass in g: needed to convert mass densities to number densities.
pmg :: FP  
pmg = 1.67262e-24

-- | Lower limit for event selection.
tiny :: FP
tiny = 1e-300

-- | Upper limit for event selection.
huge :: FP
huge = 1e+300

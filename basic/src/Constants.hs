module Constants where

import Numerical

-- | Speed of light in vacuum.
c :: FP
-- c = 1.0 -- nice for testing
c = 2.9979e10 -- cm/sec

-- | Lower limit for event selection.
tiny :: FP
tiny = 1e-300

-- | Upper limit for event selection.
huge :: FP
huge = 1e+300

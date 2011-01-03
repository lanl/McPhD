
-- Testing libraries
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- The library under test
import Tallies

-- Dependencies
import Space
import RandomValues
import RandomParticle

tests = [
        testGroup "Event data extraction" [
           testProperty "Scatters" prop_Scatters
           ]
        ]
        

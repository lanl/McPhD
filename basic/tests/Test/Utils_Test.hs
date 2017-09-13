-- Utils_Test.hs
-- T. M. Kelley
-- Mar 27, 2012
-- (c) Copyright 2012 LANSLLC, all rights reserved

module Test.Utils_Test where

-- the module to test:
import Utils

-- Testing libraries
import Test.Framework (testGroup,Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Arbitraries

prop_chunkByOutputLength :: Chunkable -> Bool
prop_chunkByOutputLength (Chunkable ls xs) =
  length chunked == length ls
    where chunked = chunkBy ls xs

tests :: [Test]
tests =
  [ testGroup "Utilities"
    [
     testProperty "chunked list has correct length" prop_chunkByOutputLength
    ]
  ]
-- End of file

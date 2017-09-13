{- Testing Tallies -}

module Test.Tally_Test (tests) where

import Test.Framework (testGroup,Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified TallyIM as IM
import Test.Arbitraries ()
import Data.Serialize

prop_serDeserEC :: IM.EventCount -> Bool
prop_serDeserEC t = case (decode . encode $ t)  of
                      Left _ -> False
                      Right t' -> t == t'

prop_serDeserEscC :: IM.EscapeCount -> Bool
prop_serDeserEscC t = case (decode . encode $ t)  of
                        Left _ -> False
                        Right t' -> t == t'

prop_serDeserCT :: IM.CellTally -> Bool
prop_serDeserCT t = case (decode . encode $ t)  of
                      Left _ -> False
                      Right t' -> t == t'

prop_serDeserPT :: IM.PhysicsTally -> Bool
prop_serDeserPT t = case (decode . encode $ t)  of
                      Left _ -> False
                      Right t' -> t == t'

prop_serDeserTallyIM :: IM.Tally -> Bool
prop_serDeserTallyIM t = case (decode . encode $ t)  of
                           Left _ -> False
                           Right t' -> t == t'

-- aggregate tests
tests :: [Test]
tests =
  [testGroup "TallyIM"
   [
    testProperty "TallyIM.EventCount   serializes/deserializes correctly" prop_serDeserEC
  , testProperty "TallyIM.CellTally    serializes/deserializes correctly" prop_serDeserCT
  , testProperty "TallyIM.EscapeCount  serializes/deserializes correctly" prop_serDeserEscC
  , testProperty "TallyIM.PhysicsTally serializes/deserializes correctly" prop_serDeserPT
  , testProperty "TallyIM.Tally        serializes/deserializes correctly" prop_serDeserTallyIM
   ]
  ]


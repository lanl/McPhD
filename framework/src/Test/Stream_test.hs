module Test.Stream_test where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

-- The libraries under test
import MonteCarlo

-- Dependencies
import Properties


-- A particle that holds an integer counter.
data Counter = Counter { counter :: Int } deriving Show
data CEvent = Decrmt | Done

-- Reduce counter to zero, then stop
countStep :: Counter -> Outcome CEvent Counter
countStep p@(Counter c) = if (c>1) 
                          then Outcome (Distance 1.0) Decrmt (Counter $ c-1) 
                          else Outcome (Distance 0.0) Done p

countCont :: CEvent -> Bool
countCont Decrmt = True
countCont Done = False

countStream = stream countStep countCont



data Collatz = Collatz { value :: Int } deriving Show
data CollatzEvent = Up | Down | One

collatzCont :: CollatzEvent -> Bool
collatzCont One = False
collatzCont _   = True

collatzStep :: Collatz -> Outcome CollatzEvent Collatz
collatzStep (Collatz v)
  | v == 2    = Outcome (Distance 1.0) One (Collatz 1)
  | odd v     = Outcome (Distance $ fromIntegral $ 2*v+1) Up (Collatz $ 3*v + 1)
  | otherwise = Outcome (Distance $ fromIntegral $ v `div` 2) Down (Collatz $ v `div` 2)

collatzStream = stream collatzStep collatzCont

tests = [ testCase "Event list length" ((length $ countStream (Counter 10)) @?= 10)
        , testCase "Collatz 2" ((length $ collatzStream (Collatz 2)) @?= 1)
        , testCase "Collatz 3" ((length $ collatzStream (Collatz 3)) @?= 7)
        ]
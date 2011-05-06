module Test.Stream_test where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

-- The libraries under test
import Stream

-- Their dependencies



-- A particle that holds an integer counter.
data Counter = Counter { counter :: Int }
data CEvent = Decrmt | Done


-- Reduce counter to zero, then stop
step :: Counter -> (CEvent, Counter)
step p@(Counter c) = if (c>1) then (Decrmt, Counter $ c-1) else (Done, p)

cont :: CEvent -> Bool
cont Decrmt = True
cont Done = False

countStream = stream step cont


data Collatz = Collatz { value :: Int } deriving Show
data CollatzEvent = Up | Down | One

collatzCont :: CollatzEvent -> Bool
collatzCont One = False
collatzCont _    = True

collatzStep :: Collatz -> (CollatzEvent, Collatz)
collatzStep (Collatz v)
  | v==2      = (One,  Collatz 1)
  | odd v     = (Up,   Collatz $ 3*v + 1)
  | otherwise = (Down, Collatz $ v `div` 2)

collatzStream = stream collatzStep collatzCont

tests = [ testCase "Event list length" ((length $ countStream (Counter 10)) @?= 10)
        , testCase "Collatz 2" ((length $ collatzStream (Collatz 2)) @?= 1)
        , testCase "Collatz 3" ((length $ collatzStream (Collatz 3)) @?= 7)
        ]
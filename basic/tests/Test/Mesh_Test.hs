-- | A test module for Sphere1D
module Test.Mesh_Test (tests) where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck
import Test.Arbitraries

-- The library under test
import Sphere1D
import Mesh
import Cell
import Physical
import PRNG
import Material
import qualified Data.Vector as V 
import Data.List (zip6)
import SoftEquiv
import Test.Sphere1D_d_to_b_TestCases

-- helpers
import Control.Applicative
import Debug.Trace


-- * samplePositionInCell: (1) pos >= lower boundary, 
-- (2) pos <= upper boundary
psnGELowBnd :: Sphere1D -> Cell -> URD -> Bool
psnGELowBnd s c xi = let
  p  = samplePositionInCell s c xi
  lb = pos . lowB $ c
  x  = pos p
  in x >= lb

psnLEHighBnd :: Sphere1D -> Cell -> URD -> Bool
psnLEHighBnd s c xi = let
  p  = samplePositionInCell s c xi
  hb = pos . highB $ c
  x  = pos p
  in x <= hb

-- * sampleDirection: direction cosine must be > -1.0 and < 1.0
dirBnded :: Sphere1D -> URD -> Bool
dirBnded s xi = let (Direction o) = sampleDirectionIso s xi
  in -1.0 <= o && o <= 1.0

-- | distance to boundary >= 0. 
dToBoundGE0 :: Sphere1D -> PositionInCell -> Direction -> Bool
dToBoundGE0 s (PiC c p) o = d >= 0.0
  where (Distance d, _) = distanceToBoundary s c p o

-- * distance to boundary test cases
matl = Material (Velocity 0) (Temperature 0) (Density 0) (NDensity 0) (NDensity 0)

clls = map mkCell d2bcases
  where mkCell (x,o,rLo,rHi,dexp,fexp) = 
          Cell (Position rLo) (Position rHi) Transp Transp matl
xs = map (\(x,_,_,_,_,_) -> Position x) d2bcases
os = map (\(_,o,_,_,_,_) -> Direction o) d2bcases
ds = map (\(_,_,_,_,d,_) -> Distance d) d2bcases
fs = map fac d2bcases
  where fac (_,_,_,_,_,fi) | fi == 1 = Lo
                           | fi == 2 = Hi

d2bData = zip6 [1..length xs] xs os clls ds fs
 
d2bStr :: Int -> String
d2bStr i = "d2b case " ++ show i

d2bFailStr :: Int -> Position -> Direction -> Position -> Position ->
              Distance -> Face -> Distance -> Face -> String
d2bFailStr i (Position x) (Direction o) (Position rl) 
             (Position ro) (Distance dexp) fexp
             (Distance dact) fact =
  "distanceToBdy case " ++ show i ++ " failed, x: " ++ show x ++ 
    ", direction: " ++ show o ++ ", rLow: " ++ show rl ++
    ", rHigh: " ++ show ro ++ "; d_exp: " ++ show dexp ++
    ", d_act: " ++ show dact ++ ", f_exp: " ++ show fexp ++
    ", f_act: " ++ show fact

d2bTest :: Int -> Position -> Direction -> Cell ->
           Distance -> Face -> Assertion
d2bTest i x o cll@(Cell {lowB = rl,highB = rh}) dexp fexp = 
  assertBool (d2bFailStr i x o rl rh dexp fexp dact fact)
             ((softEquiv dexp dact (Distance 5e-14)) &&
             (fexp == fact))
  where (dact,fact) = distanceToBoundary msh cll x o
        msh = Sphere1D $ V.fromList [cll]

d2bCases = [testCase (d2bStr i) (d2bTest i x o cll dexp fexp) |
           (i,x,o,cll,dexp,fexp) <- d2bData]


tests = [testGroup "Sphere1D" 
         [ testProperty "samplePosInCell: x >= cell lower bound" psnGELowBnd
         , testProperty "samplePosInCell: x <= cell upper bound" psnLEHighBnd
         , testProperty "sampleDirection: -1 < omega < 1" dirBnded
         , testProperty "distanceToBoundary: d >= 0" dToBoundGE0
         ]
        , testGroup "Sphere1D: distanceToBoundary cases" d2bCases
        ]

-- PickEvent_Test.hs
-- T. M. Kelley
-- Aug 08, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Test.PickEvent_Test (tests) where

-- The library under test
import Material
import MC
import Particle as P
import Physical
import Sigma_HBFC
import SoftEquiv
import Sphere1D

import Test.Framework (testGroup,Test)
import Test.Framework.Providers.API (Test)
import Test.Framework.Providers.HUnit
import Test.HUnit

-- helpers
import qualified Data.Vector as V
import Data.List (zip4,unfoldr)
import Control.Applicative ()

type HTest = Test.Framework.Providers.API.Test
type TTest = Test.Framework.Test
-- | exports from this module
tests :: [TTest]
tests = [ testGroup "pickEvent" pECases]


-- * pickEvent
pECases :: [HTest]
pECases = [
    testCase "pickEvent: stream 0.5 cm to high boundary" pETest1
  , testCase "pickEvent: stream through 10 cells, escape on last one" pETest2
  , testCase "pickEvent: Nucleon absorption" pETest3
  , testCase "pickEvent: Nucleon elastic scatter" pETest4
  , testCase "pickEvent: e- scatter" pETest5
  ]

-- These test cases changed quite a bit upon refactoring
-- MC. I've left a bunch of the old expressions here--they should
-- be incorporated into new cases that test the new parts of MC.

pETest1 :: Assertion
pETest1 =
  let
    msh = genMesh 10 1.0
    -- rng = makeFakeGen $ cycle [0.30897681610609407,
    --                            0.92436920169905545,
    --                            0.21932404923057958]
    p = Particle (Position 0.5) (Direction 1) (Time 1) (Energy 5) (EnergyWeight 1) (CellIdx 0)
    event = pickEvent nuE msh p (URD 0.30897681610609407)
    -- event = runRnd rng (pickEvent nuE msh p)
    eventExp = BoundaryCand (Distance 0.5) Hi
    -- eventExp = Boundary Transmit (Distance 0.5) Hi (Energy 5) (EnergyWeight 1)
  in assertBool ("chosen event, " ++ show event ++
                  ", did not match expected event, " ++
                  show eventExp)
                (event == eventExp)

{- This case runs a particle through a 10 cell mesh. On the last step, -
 - it should escape. Not the most convincing test in the world, meh.   -}
pETest2 :: Assertion
pETest2 =
  let
    msh = genMesh 10 1.0
    -- rng = makeFakeGen $ cycle [0]
    p0 = Particle (Position 0.5) (Direction 1) (Time 1) (Energy 5) (EnergyWeight 1) (CellIdx 0)
    ps = take 10 (unfoldr pf p0)
    -- pf: evolve particle state
    pf :: Particle -> Maybe (Particle,Particle)
    pf p@(Particle {P.pos = Position x,cellIdx = CellIdx cidx}) = Just $
      (p, p{ P.pos        = Position (x + d), cellIdx = CellIdx  (cidx + 1)})
      where (Distance d)  = candDist evt
            evt           = pickEvent nuE msh p (URD 0)
    p10 = last ps
    event = pickEvent nuE msh p10 (URD 0)
    -- event = runRnd rng (pickEvent nuE msh p10)
    eventExp = BoundaryCand (Distance 1.0) Hi
    -- eventExp = Boundary Escape (Distance 1.0) Hi (Energy 5) (EnergyWeight 1)
  in assertBool ("chosen event, " ++ show event ++
                  ", did not match expected event, " ++
                  show eventExp)
                (event == eventExp)

{- runs a particle through a cell with reasonable nucleon density; RNG -
 - stacked to select nucleon absorption. Note that the sequence of RN  -
 - consumption is different from the Python/C++ version. -}
pETest3 :: Assertion
pETest3 =
  let
    n    = 10
    dx   = 1e6
    clls = genCells (genBndConds n) (genBounds n dx) denseNMat
    msh  = Sphere1D . V.fromList $ clls
    -- rng  = makeFakeGen $ cycle [0.30897681610609407,    -- this is used for d_coll
    --                          -- 0.92436920169905545, -- not used in Haskell
    --                             0.21932404923057958]
    p = Particle (Position 0.5) (Direction 1) (Time 100) (Energy 5) (EnergyWeight 1) (CellIdx 0)
    event = pickEvent nuE msh p (URD 0.30897681610609407)
    -- event = runRnd rng (pickEvent nuE msh p)
    dExp  = 7343.827
    -- mExp  = 5/c
    -- eExp  = 5
    eventExp = CollisionCand (Distance dExp)
    d = Physical.distance . candDist $ event
    -- eventExp = Collision NuclAbs (Distance dExp) (Momentum mExp) (Energy eExp)
  in assertBool ("chosen event, " ++ show event ++
                  ", did not match expected event, " ++
                  show eventExp)
                (softEquiv d dExp 1e-7)
                -- (case event of
                --    Collision NuclAbs (Distance d) (Momentum pdep) (Energy edep) ->
                --      softEquiv d dExp 1e-3 &&
                --      softEquiv pdep mExp 1e-10 &&
                --      eExp == edep
                --    _ -> False
                -- )

{- runs a particle through a cell with reasonable nucleon density; RNG -
 - stacked to select nucleon scattering. Note that the sequence of RN  -
 - consumption is different from the Python/C++ version. -}
pETest4 :: Assertion
pETest4 =
  let
    n    = 10
    dx   = 1e6
    clls = genCells (genBndConds n) (genBounds n dx) denseNMat
    msh  = Sphere1D . V.fromList $ clls
    -- rng  = makeFakeGen $ cycle [0.21932404923057958, -- used for d_coll
    --                          -- 0.20867489035315723,
    --                             0.91525579001682567]
    p = Particle (Position 0.5) (Direction 1) (Time 100) (Energy 5) (EnergyWeight 1) (CellIdx 0)
    event = pickEvent nuE msh p (URD 0.21932404923057958)
    -- event = runRnd rng (pickEvent nuE msh p)
    dExp  = 9486.756524
    -- mExp  = 5/c
    -- eExp  = 0
    eventExp = CollisionCand (Distance dExp)
    -- eventExp = Collision NuclEl (Distance dExp) (Momentum mExp) (Energy eExp)
    d = Physical.distance . candDist $ event
  in assertBool ("chosen event, " ++ show event ++
                  ", did not match expected event, " ++
                  show eventExp)
                (softEquiv d dExp 1e-9)
                -- (case event of
                --    Collision NuclEl (Distance d) (Momentum pdep) (Energy edep) ->
                --      softEquiv d dExp 1e-3 &&
                --      -- softEquiv pdep mExp 1e-10 &&
                --      eExp == edep
                --    _ -> False
                -- )

{- runs a particle through a cell with reasonable lepton density; RNG -
 - stacked to select lepton scattering. Note that the sequence of RN  -
 - consumption is different from the Python/C++ version. -}
pETest5 :: Assertion
pETest5 =
  let
    n    = 10
    dx   = 1e6
    clls = genCells (genBndConds n) (genBounds n dx) denseEMinusMat
    msh  = Sphere1D . V.fromList $ clls
    -- rng  = makeFakeGen $ cycle [0.9, -- used for d_coll
    --                          -- 0.9,
    --                             0.1]
    p = Particle (Position 0.5) (Direction 1) (Time 100) (Energy 5) (EnergyWeight 1) (CellIdx 0)
    xi = URD 0.9
    event = pickEvent nuE msh p xi
    -- event = runRnd rng (pickEvent nuE msh p xi)
    dExp  = 38310.45776
    -- mExp  = 1/c
    -- eExp  = 1
    eventExp = CollisionCand (Distance dExp)
    -- eventExp = Collision EMinusInel (Distance dExp) (Momentum mExp) (Energy eExp)
    d = Physical.distance . candDist $ event
  in assertBool ("chosen event, " ++ show event ++
                  ", did not match expected event, " ++
                  show eventExp)
                (softEquiv d dExp 1e-9)
                -- (case event of
                --    Collision EMinusInel (Distance d) (Momentum pdep) (Energy edep) ->
                --      softEquiv d dExp 1e-3 &&
                --      -- softEquiv pdep mExp 1e-10 &&
                --      eExp == edep
                --    _ -> False
                -- )

-- helpers
genBndConds :: Int -> [BoundaryCondition]
genBndConds n = [Refl] ++ [Transp | _<-[1..n-1]] ++ [Vac]

genBounds :: Int -> Double -> [Position]
genBounds   n dx = [Position $ dx * fromIntegral i | i <- [0..n]]

zeroMat :: Material
zeroMat = Material (Velocity 0) (Temperature 0)
          (Density 1.0) (NDensity 0) (NDensity 0)

denseNMat :: Material
denseNMat = Material (Velocity 0) (Temperature 0)
            (Density 1e14) (NDensity 0) (NDensity 0)

denseEMinusMat :: Material
denseEMinusMat = Material (Velocity 0) (Temperature 1)
                 (Density 0) (NDensity (1e14/pmg)) (NDensity 0)

genCells :: [BoundaryCondition] -> [Position] -> Material -> [Cell]
genCells bcs xs matl = [Cell posl posh bcl bch matl  |
                        (posl,posh,bcl,bch) <- zip4 (init xs)
                                                    (tail xs)
                                                    (init bcs)
                                                    (tail bcs)]

genMesh :: Int -> Double -> Sphere1D
genMesh n dx = Sphere1D . V.fromList $
               genCells (genBndConds n) (genBounds n dx) zeroMat


-- end of file

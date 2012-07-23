module Test.MC_test where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck
import Test.TestingTools

-- The library under test
import MC

-- It's dependencies
import Mesh
import Particle
import Numerical
import Physical
import Material
import Event
import PRNG

getEvent :: Mesh -> Material -> Particle -> Unit FP -> Unit FP -> Direction -> Event
getEvent mesh matl p (Unit s1) (Unit s2) direction = pickEvent p s1 s2 direction matl mesh
 
meshSize :: (CellIdx, CellIdx)
meshSize = (1,2)

mshType  = Sphere1D
testMesh :: Mesh
testMesh = mshType $ listArray meshSize
           [
            CellProps  (Position 0.0) (Position 1.0) (bc1D Refl) (bc1D Transp)
           , CellProps (Position 1.0) (Position 2.0) (bc1D Transp) (bc1D Refl)
           ]

noScatteringMaterial = Material $ listArray meshSize $ repeat (MatState (Opacity 0.0) (Opacity 0.0) (Velocity 0.0) (Temperature 1.0))

-- | In a test problem without scattering, all events should be face crossings
prop_AllSurfXings = propFunction testMesh noScatteringMaterial

-- | This function can be used to create multiple property functions
-- by closing over the mesh and material variables.
propFunction :: Mesh -> Material -> RNG -> Tag -> Unit FP -> Unit FP -> Bool
propFunction mesh matl rng tag u1 u2 = 
--   let particle = sampleParticle mesh rng tag
  let particle = sampleParticle mesh rng
      event    = getEvent mesh matl particle u1 u2 (pDir particle) 
  in case event of
       Escape   _ _ -> True
       Reflect  _ _ -> True
       Transmit _ _ -> True
       _            -> False

-- sampleAndGetEvent :: Mesh -> Material -> RNG -> Tag -> Event
-- sampleAndGetEvent mesh matl rng tag =
--     let (u1, rng')  = (Unit x, rng') where (x, rng')  = randomDouble rng
--         (u2, rng'') = (Unit y rng'') where (y, rng'') = randomDouble rng'
--         particle = sampleParticle mesh rng'' tag
--     in getEvent mesh matl particle u1 u2 (pDir particle)

tests = [ testGroup "Events"
          [ 
           testProperty "No scattering or census -> All crossings" 
                        prop_AllSurfXings
          ]
        ]

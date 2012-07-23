-- | An executable to run all of the tests
import Test.Framework (defaultMain, testGroup, Test)

-- Modules under test:

import Test.Collision_Test   as Collision
import Test.Geometry_Test    as Geometry
import Test.Histogram_Test   as Histogram
import Test.Mesh_Test        as Mesh
import Test.Opacity_Test     as Opacity
import Test.Partition_Test   as Partition
import Test.Philox_Test      as Philox
import Test.PickEvent_Test   as PickEvent
import Test.Source_Test      as Source
import Test.Stream_Test      as Stream
import Test.Tally_Test       as Tally
import Test.Utils_Test       as Utils

-- import Test.Sphere1D_test as Sphere1D
-- import Test.MC_test as MC

all_tests :: [Test]
all_tests = [ testGroup "Mesh tests"      Mesh.tests
            , testGroup "Geometry Tests"  Geometry.tests
            , testGroup "Opacity Tests"   Opacity.tests
            , testGroup "Collision Tests" Collision.tests
            , testGroup "Source Tests"    Source.tests
            , testGroup "PickEvent Tests" PickEvent.tests
            , testGroup "Stream Tests"    Stream.tests
            , testGroup "Histogram"       Histogram.tests
            , testGroup "Philox"          Philox.tests
            , testGroup "Tally"           Tally.tests
            , testGroup "Partition"       Partition.tests
            , testGroup "Utilities"       Utils.tests
            -- , testGroup "Sphere1D tests" Sphere1D.tests
            -- , testGroup "MC tests"       MC.tests
            ]

main :: IO ()
main = defaultMain all_tests

import MC
import Particle
import Mesh
import Material
import Physical
import Numerical

-- import Data.Word


main :: IO ()
main = runOne

-- runABunch :: Int -> IO ()
-- let xs = [x | x <- rand]

runOne :: IO ()
runOne = do
  let x     = Position 0.5
  let w     = Direction 1.0
  let t     = Time 1.5
  let nrg   = Energy 1.0
  let wt    = EnergyWeight 1.0
  let cell  = CellIdx 1
  let tag   = 42 :: Word32
  let p = Particle x w t nrg wt cell (RNG rand) tag
  ecounts <- runParticleV p rand infMesh simpleMat
  print ecounts

-- smBounds = (1::CellIdx,2::CellIdx)

infMesh :: Mesh
infMesh = Sphere1D $ listArray (1,2)
          [(CellProps (Position 0.0) (Position 1.0) (bc1D Refl) (bc1D Transp)),
           (CellProps (Position 1.0) (Position 2.0) (bc1D Transp) (bc1D Refl))]

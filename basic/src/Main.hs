import MC
import PRNG
import Particle
import Mesh
import Material
import Physical
import Tally
import Source
import Control.Monad
import TryNSave

import System (getArgs)

main :: IO ()
main = do
  n <- parseCL
  let tally = runManyP infMesh simpleMat n
  writeTally "tally1" tally

runManyP :: Mesh -> Material -> Word32 -> Tally
runManyP msh mat ntot = let 
  ps = genParticles ntot msh prand
  tallies = map (runParticle msh mat) ps
  in foldr merge emptyTally tallies

-- mshType = Cart1D
mshType = Sphere1D

infMesh :: Mesh
infMesh = mshType $ listArray (1,2)
          [CellProps (Position 0.0) (Position 1.0) (bc1D Refl) (bc1D Transp),
           CellProps (Position 1.0) (Position 2.0) (bc1D Transp) (bc1D Refl)]

-- try tuning the scattering and absorption opacities in each cell
simpleMat :: Material
simpleMat =  
  Material $ listArray (1,2) 
    [ MatState (Opacity 0.1) (Opacity 2.0) (Velocity 0.0) (Temperature 1.0),
      MatState (Opacity 1.0) (Opacity 0.5) (Velocity 0.0) (Temperature 2)]

parseCL :: IO Word32
parseCL = do
  args <- getArgs
  let n  = head args
      wn = read n :: Word32
  if wn <= 0
     then error ("first command line argument (n) must be greater than zero,"++
              "you specified n = " ++ show wn ++ " which is obviously <= 0.")
     else return wn
     
-- End of file

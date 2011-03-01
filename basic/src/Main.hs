import MC
import Particle
import Mesh
import Material
import Physical
import Tally
import Source
import Control.Monad
import TryNSave

import System (getArgs)

-- main :: IO ()
-- main = runOne

main :: IO ()
main = do
  n <- parseCL
  tally <- runABunch infMesh simpleMat n
  writeTally tally "tally1"

runABunch :: Mesh ->
             Material ->
             Word32 -> 
             IO Tally
runABunch msh mat ntot = do
  let t = emptyTally 
      runner = runParticleQ_ msh mat
  ps <- genParticles ntot msh rand
  tallies <- forM ps runner
  let tally = foldr merge t tallies
  return tally

-- runOne :: IO ()
-- runOne = do
--   let x     = Position 0.5
--       w     = Direction 1.0
--       t     = Time 1.5
--       nrg   = Energy 1.0
--       wt    = EnergyWeight 1.0
--       cell  = CellIdx 1
--       tag   = 42 :: Word32
--       p     = Particle x w t nrg wt cell rand tag
--   ecounts <- runParticleV p infMesh simpleMat
--   print ecounts

-- infMesh :: Mesh
-- infMesh = Cart1D $ listArray (1,2)
--           [(CellProps (Position 0.0) (Position 1.0) (bc1D Refl) (bc1D Transp)),
--            (CellProps (Position 1.0) (Position 2.0) (bc1D Transp) (bc1D Refl))]

infMesh :: Mesh
infMesh = Sphere1D $ listArray (1,2)
          [(CellProps (Position 0.0) (Position 1.0) (bc1D Refl) (bc1D Transp)),
           (CellProps (Position 1.0) (Position 2.0) (bc1D Transp) (bc1D Refl))]

-- try tuning the scattering and absorption opacities in each cell
--                                           scat  abs
simpleMat :: Material
simpleMat  =  Material $ listArray (1,2) 
              [ MatState (Opacity 0.1) (Opacity 2.0) (Velocity 0.0) (Temperature 1.0),
                MatState (Opacity 1.0) (Opacity 0.5) (Velocity 0.0) (Temperature 2)]

parseCL :: IO Word32
parseCL = do
  args <- getArgs
  let n  = head args
      wn = read n :: Word32
  if wn <= 0
     then error ("first command line argument (n) must be greater than zero,"++
              "you specified n = " ++ (show wn) ++ " which is obviously <= 0.")
     else return wn
     

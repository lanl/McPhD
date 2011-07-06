-- TryNSave.hs
-- T. M. Kelley
-- Feb 28, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module TryNSave (writeTally
                ,summarizeTally
                ,readMatStateP)
    where

import Tally
import Cell
import Text.CSV
import Physical
import Data.List (zip4)
import qualified Data.Vector.Unboxed as V
import Constants (k_B,pmg)
import Material
import Particle
-- import Control.Monad

writeTally :: String -> Tally -> IO ()
writeTally name = writeFile name . show 

summarizeTally :: Tally -> IO ()
summarizeTally (Tally cntrs dep esc) = do
  -- summarize global events
  let (Energy totEDep) = V.sum (V.map (\ct -> ctEnergy ct) dep)
      (Momentum totMDep) = V.sum (V.map (\ct -> ctMom ct) dep)
  mapM_ putStrLn $ 
          ("Total energy deposited: " ++ show totEDep) :
          "Scatters:" :
           ("\tnucleon elastic: " ++ show (nNuclEl cntrs)) :
           ("\tnu_e--electron scatters: " ++ show (nEMinusInel cntrs)) :
           ("\tnu_e_bar--positron scatters: " ++ show (nEPlusInel cntrs)) :
          "\tnu_x--electron scatters: FIX ME!"  :
          "\tnu_x_bar--positron scatters: FIX ME!"  :
          "Absorptions:" :
           ("\tnu_i nucleon absorptions: " ++ show (nNuclAbs cntrs)) :
          "Mesh:" :
           ("\tcell boundary crossings: " ++ show (nTransmit cntrs)) :
           ("\treflections: " ++ show (nReflect cntrs)) :
           ("\tescapes: " ++ show (nEscape cntrs)) :
          "Timeouts:" :
           ("\ttimeouts: " ++ show (nTimeout cntrs)) :
          "==================================================" : []

type CellGeom = (Position,Position,BoundaryCondition,BoundaryCondition)

-- | Read a material state file. Cell coordinates get special treatment,
-- everything else is read out, possibly with a scalar conversion. This 
-- is specialized to one particular format.
readMatStateP :: FilePath -> IO ([Cell],[Luminosity],[Luminosity],[Luminosity])
readMatStateP f = do
  elines <- parseCSVFromFile f
  case elines of 
    Left  _  -> return ([],[],[],[])
    Right ls -> do
      let recs = init ls
          bs = cellBoundsP recs
      return $ (getCellsP recs bs, getLumsP recs NuE, getLumsP recs NuEBar, 
                getLumsP recs NuX) 


getLumsP :: [Record] -> PType -> [Luminosity]
getLumsP rs NuE    = map f rs
  where f = (\r->Luminosity $ 1e51 * (read (r!!10) + read (r!!14)))
getLumsP rs NuEBar = map f rs
  where f = (\r->Luminosity $ 1e51 * (read (r!!12) + read (r!!16) ))
getLumsP rs NuX = map f rs
  where f = (\r->Luminosity $ 1e51 * (read (r!!18) ))

getCellsP :: [Record] -> [CellGeom] -> [Cell]
getCellsP recs gs = map getCellP (zip recs gs)
        
getCellP :: (Record,CellGeom) -> Cell
getCellP (rcd,(lox,hix,lobc,hibc)) = Cell lox hix lobc hibc mtl
  where mtl = Material op0 op0 velo tmp rhoN rhoEM rhoEP
        op0 = Opacity 0.0
        velo = Velocity $ read (rcd!!4)
        tmp = Temperature . (k_B*1e9*) $ read (rcd!!7)
        rhoN = Density $ read (rcd!!3)
        rhoEM = NDensity $ (rho rhoN) / pmg * read (rcd!!5)
        rhoEP = NDensity 0.0 -- no positrons in the file.

-- | Process cell centers given in data file into pairs of (lower,upper)
-- bounds. Assume that the bound of a cell is halfway between successive 
-- centers. For lower bound of first cell, take difference between cell
-- 0 and cell 1, and similarly for last cell.
cellBoundsP :: [Record] -> 
               [CellGeom]
cellBoundsP rs = bounds
  where bounds = zip4 lowerBounds upperBounds lowerBCs upperBCs
        lowerBounds = map Position $ zipWith (-) centers (init ds)
        upperBounds = map Position $ zipWith (+) centers (tail ds)
        lowerBCs = init bcs
        upperBCs = tail bcs
        ds      = d0 : (map (0.5*) diffs) ++ [dN]
        diffs   = zipWith (-) (tail centers) (init centers)
        centers = map (read.(!!2)) rs :: [Double]
        d0      = (centers !! 1 - centers !! 0) / 2.0
        dN      = (centers !! (n-1) - centers !! (n-2)) / 2.0
        bcs     = Refl : [Transp | _ <- [1..(n-1)]] ++ [Vac]
        n       = length centers

-- -- handy utility for testing
-- csr cs = case cs of
--            Left err -> []
--            Right rs -> rs

-- -- how to do this using bind?
-- rsr f = do 
--   cs <- parseCSVFromFile f 
--   return $ csr cs

-- version
-- $Id$

-- End of file

-- TryNSave.hs
-- T. M. Kelley
-- Feb 28, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module TryNSave (writeTally
                ,writeHistogram
                ,summarizeTally
                ,summarizeTallyIO
                ,summarizeStats
                ,readMatStateP)
    where

import Tally
import Cell
import Physical
import Material
import Particle
import Source
import Histogram

import Text.CSV
import Text.Printf
import Data.List as L
import Data.Monoid ( (<>) )
import qualified Data.Vector.Unboxed as V

import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Char8 as C
type CBS = C.ByteString

summarizeStats :: [SrcStat] -> PType -> IO ()
summarizeStats stats typ = do
  let sz     = length stats
      ntot   = sum (map (\(_,b,_,_) -> b) stats)
      meanEW = sum (map (\(_,_,c,_) -> c) stats)
      etot   = sum (map (\(_,_,_,d) -> d) stats)
      fmtstr = "For %s:\n\t%i particles\n\t%e total energy\n\t%e mean energy weight"
      outstr = printf fmtstr (show typ) ntot (e etot) (ew meanEW / fromIntegral sz)
  putStrLn outstr

writeHistogram :: FilePath -> EHist -> IO ()
writeHistogram fname (EHist cnts sqrs ns bns) = 
  let bnsBS  = (writeVector bns  "%15e " "bins") <> C.pack "\n"
      cntsBS = (writeVector cnts "%15e " "weight")  <> C.pack "\n"
      sqrsBS = (writeVector sqrs "%15e " "sum_of_squared_weights") <> C.pack "\n"
      nsBS   = (writeVector ns   "%d " "number_of_events") <> C.pack "\n"
  in C.writeFile fname (bnsBS <> cntsBS <> sqrsBS <> nsBS)

printfx :: PrintfArg a => String -> String -> a -> String
printfx f s x = s ++ (printf f x)

writeVector :: (V.Unbox a, PrintfArg a) => V.Vector a -> String -> String -> CBS
writeVector v fmt name = 
  let bs = C.pack $ V.foldl' (printfx fmt) "" v
  in C.pack name <> C.pack "=[" <> bs <> C.pack "]" 

writeTally :: String -> Tally -> IO ()
writeTally name t = let header     = summarizeTally t 
                        deposition = writeDeposition t
                    in C.writeFile name (header `C.append` deposition)

appCellTally :: CBS -> (Int,CellTally) -> CBS
appCellTally s (cidx,CellTally (Momentum m) (Energy e)) = s `C.append` line
  where line   = C.pack $ printf fmtStr cidx m e
        fmtStr = "%04i: %15e, %15e\n"

writeDeposition :: Tally -> CBS
writeDeposition (Tally _ dep _ _) = depHeader `C.append` lines
  where lines = L.foldl' appCellTally C.empty cells
        depHeader = C.pack "\nCell        Momentum                Energy    \n"
        cells = sortBy (\(k1,_) (k2,_) -> compare k1 k2) $ Map.toList dep

-- TO DO: better treatment of escape tally?
summarizeTally :: Tally -> CBS
summarizeTally tlly@(Tally cntrs _dep _ (Distance pl)) = 
  let CellTally{ctMom = Momentum momTot,ctEnergy = Energy eTot} = totalDep tlly
  in C.intercalate (C.pack "\n") . map (C.pack) $
           ("Total energy deposited: "        ++ show eTot) :
           ("Net radial momentum deposited: " ++ show momTot) :
           ("Total path length: "             ++ show pl) :
          "Scatters:" :
           ("\tnucleon elastic: "             ++ show (nNuclEl cntrs)) :
           ("\telectron scatters: "           ++ show (nEMinusInel cntrs)) :
           ("\tpositron scatters: "           ++ show (nEPlusInel cntrs)) :
          "Absorptions:" :
           ("\tnu_i nucleon absorptions: "    ++ show (nNuclAbs cntrs)) :
          "Mesh:" :
           ("\tcell boundary crossings: "     ++ show (nTransmit cntrs)) :
           ("\treflections: "                 ++ show (nReflect cntrs)) :
           ("\tescapes: "                     ++ show (nEscape cntrs)) :
          "Timeouts:" :
           ("\ttimeouts: "                    ++ show (nTimeout cntrs)) :
           ("Total number of MC steps: "      ++ show (totalMCSteps cntrs)) :
          "==================================================" : []

-- TO DO: better treatment of escape tally?
summarizeTallyIO :: Tally -> IO ()
summarizeTallyIO tlly@(Tally cntrs _dep _ pl) = do
  -- summarize global events
  let CellTally{ctMom = momTot,ctEnergy = eTot} = totalDep tlly
  mapM_ putStrLn $
           ("Total energy deposited: "        ++ show eTot) :
           ("Net radial momentum deposited: " ++ show momTot) :
           ("Total path length: "             ++ show pl) :
          "Scatters:" :
           ("\tnucleon elastic: "             ++ show (nNuclEl cntrs)) :
           ("\telectron scatters: "           ++ show (nEMinusInel cntrs)) :
           ("\tpositron scatters: "           ++ show (nEPlusInel cntrs)) :
          "Absorptions:" :
           ("\tnu_i nucleon absorptions: "    ++ show (nNuclAbs cntrs)) :
          "Mesh:" :
           ("\tcell boundary crossings: "     ++ show (nTransmit cntrs)) :
           ("\treflections: "                 ++ show (nReflect cntrs)) :
           ("\tescapes: "                     ++ show (nEscape cntrs)) :
          "Timeouts:" :
           ("\ttimeouts: "                    ++ show (nTimeout cntrs)) :
           ("Total number of MC steps: "      ++ show (totalMCSteps cntrs)) :
          "==================================================" : []

type CllGeom = (Position,Position,BoundaryCondition,BoundaryCondition)

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
          bs   = cellBoundsP recs
      return ( getCellsP recs bs
             , getLumsP  recs NuE
             , getLumsP  recs NuEBar
             , getLumsP  recs NuX
             )


getLumsP :: [Record] -> PType -> [Luminosity]
getLumsP rs NuE    = map f rs
  where f r = Luminosity $ 1e51 * (read (r!!10) + read (r!!14))
getLumsP rs NuEBar = map f rs
  where f r = Luminosity $ 1e51 * (read (r!!12) + read (r!!16))
getLumsP rs NuX = map f rs
  where f r = Luminosity $ 1e51 * (read (r!!18) )

getCellsP :: [Record] -> [CllGeom] -> [Cell]
getCellsP recs gs = map getCellP (zip recs gs)

getCellP :: (Record,CllGeom) -> Cell
getCellP (rcd,(lox,hix,lobc,hibc)) = Cell lox hix lobc hibc mtl
  where mtl   = Material velo tmp rhoN rhoEM rhoEP
        velo  = Velocity $ read (rcd!!4)
        tmp   = Temperature . (k_B*1e9*) $ read (rcd!!7)
        rhoN  = Density $ read (rcd!!3)
        rhoEM = NDensity $ (rho rhoN) / pmg * read (rcd!!5)
        rhoEP = NDensity 0 -- no positrons in the file.

-- | Process cell centers given in data file into pairs of (lower,upper)
-- bounds. Assume that the bound of a cell is halfway between successive
-- centers. For lower bound of first cell, take difference between cell
-- 0 and cell 1, and similarly for last cell.
cellBoundsP :: [Record] ->
               [CllGeom]
cellBoundsP rs = bounds
  where bounds = L.zip4 lowerBounds upperBounds lowerBCs upperBCs
        lowerBounds = map Position $ zipWith (-) centers (init ds)
        upperBounds = map Position $ zipWith (+) centers (tail ds)
        lowerBCs = init bcs
        upperBCs = tail bcs
        ds       = d0 : (map (0.5*) diffs) ++ [dN]
        diffs    = zipWith (-) (tail centers) (init centers)
        centers  = map (read . (!!2)) rs :: [Double]
        d0       = (centers !!    1  - centers !! 0    ) / 2
        dN       = (centers !! (n-1) - centers !! (n-2)) / 2
        bcs      = Refl : replicate (n - 1) Transp ++ [Vac]
        n        = length centers

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

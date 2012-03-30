-- FileInputCF.hs
-- Nov. 11, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

{-| Read simulation inputs from data files provided by Chris Fryer. 
 - This version attempts greater strictness via Vector. -}

module FileInputCF (parseFile) where

import qualified Data.ByteString.Lazy.Char8      as L
import qualified Data.ByteString.Lex.Lazy.Double as L
import qualified Data.Vector                     as U
import Data.Maybe          (fromMaybe)
import Control.Applicative ( (<$>) )
-- import Debug.Trace

import Cell
import Physical
import Material

type CellInfo = (Position,Velocity,Temperature,Density,NDensity)
type Lums     = (Luminosity,Luminosity,Luminosity)

type VecDub   = U.Vector Double
type VecLum   = U.Vector Luminosity
type VecCell  = U.Vector Cell
type VecGeoms = U.Vector CellGeom
type VecLine  = U.Vector (CellInfo,Lums)
type VecCellInfo = U.Vector CellInfo
type VecCellBounds = U.Vector Position

parseFile :: FilePath -> IO (VecCell,VecLum,VecLum,VecLum)
parseFile f = do 
  all <- L.readFile f 
  let lines = L.lines all
  return $ mkSim lines

-- parse each line to doubles
-- extract cell center and luminosities vectors
-- use cell centers to lay out vector of cells
mkSim :: [L.ByteString] -> (VecCell,VecLum,VecLum,VecLum)
mkSim bss = separate . interpretLs $ bss

-- parse, interpret each line to a vector
interpretLs :: [L.ByteString] -> VecLine
interpretLs = U.fromList . map (interpretL . parseL)

-- transpose & process vector of tuples into tuple of vectors.
separate :: VecLine -> (VecCell,VecLum,VecLum,VecLum)
separate vl = 
  let (vci,vlums)   = U.unzip vl
      (vl1,vl2,vl3) = U.unzip3 vlums
  in (mkVecCells vci, vl1, vl2, vl3)

-- turn a vector of cell data into a vector of cells
mkVecCells :: VecCellInfo -> VecCell
mkVecCells vci = 
  let geoms = mkGeoms ctrs
      (ctrs,_,_,_,_) = U.unzip5 vci
  in U.zipWith mkCell vci geoms

-- turn a single CellInfo + CellGeom into a Cell
mkCell :: CellInfo -> CellGeom -> Cell
mkCell ci (CellGeom lb ub lbc ubc) = Cell lb ub lbc ubc mtl
  where mtl = mkMat ci

mkMat :: CellInfo -> Material
mkMat (_,v,t,r,n) = Material v t r n (NDensity 0) -- no positrons

-- work out cell geometry. Take cell boundaries as midpoint between
-- cell centers.
mkGeoms :: VecCellBounds -> VecGeoms
mkGeoms ctrs =
    -- trace (show n) $
    U.zipWith4 CellGeom lowerBounds upperBounds lowerBCs upperBCs
  where lowerBounds = U.zipWith (-) ctrs (U.init diffs)
        upperBounds = U.zipWith (-) ctrs (U.tail diffs)
        diffs       = U.generate (n+1) (genDiff n ctrs)
        lowerBCs    = U.init bcs
        upperBCs    = U.tail bcs
        bcs         = U.generate (n+1) (genBound n)
        n           = U.length ctrs

genDiff :: Int -> VecCellBounds -> Int -> Position
genDiff _ ctrs 0 = midpoint ctrs 1 0
genDiff n ctrs i | i == n    = midpoint ctrs (n-1) (n-2)
genDiff _ ctrs i | otherwise = midpoint ctrs i (i-1)

midpoint :: VecCellBounds -> Int -> Int -> Position
midpoint cs i j = (cs U.! i - cs U.! j) / (2)

genBound :: Int -> Int -> BoundaryCondition
genBound _ 0 = Refl
genBound n i | i == n = Vac
genBound _ _ = Transp

-- convert raw doubles to cell center, velocity, tmp, rhoN,
-- rhoE, L_nu_E, L_nu_Ebar, L_nu_X
interpretL :: VecDub -> (CellInfo,Lums)
interpretL ds = (ci,ls) 
  where ci = ( Position   $ ds U.! 2
             , Velocity    $ ds U.! 4
             , Temperature $ k_B * 1e9 * (ds U.! 7)
             , Density     $ ds U.! 3
             , NDensity    $ (ds U.! 3)/pmg * (ds U.! 5)
             )
        ls = ( Luminosity  $ 1e51 * (ds U.! 10 + ds U.! 14)  -- nu_e
             , Luminosity  $ 1e51 * (ds U.! 12 + ds U.! 16)  -- nu_e_bar
             , Luminosity  $ 1e51 * (ds U.! 18)         -- nu_x
             )

-- convert ByteString for one line to doubles
parseL :: L.ByteString -> VecDub
parseL s = 
  let ws = L.split ',' s
      readFirst :: L.ByteString -> Maybe Double
      readFirst s = fst <$> L.readDouble s
  in U.fromList $ map (fromMaybe 0 . readFirst) ws

-- version
-- $Id$

-- End of file

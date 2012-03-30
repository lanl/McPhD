-- summarizeGC.hs
-- T. M. Kelley
-- Jan 06, 2012
-- (c) Copyright 2012 LANSLLC, all rights reserved

-- | summarize data from the black-hole app (though there should be nothing
-- specific to black-hole in here).
module SummarizeBH 
  where

import ParseBH

import qualified Data.ByteString.Char8 as BS

newtype Mean    = Mean Double          deriving Show
newtype StdDev  = StdDev Double        deriving Show
newtype Min     = Min Double           deriving Show
newtype Max     = Max Double           deriving Show
newtype Stat    = Stat (Mean,StdDev,Min,Max)   deriving Show

type BStr = BS.ByteString

-- * reporting: this should be independent of neutrinos, tallies, etc.

-- for each species, compute statistics for a target across all runs
istat :: [Summary] -> IntTarget -> Stat
istat rs t = statistify vals
  where vals = map (fromIntegral . itargetVal t) rs
dstat rs t = statistify vals
  where vals = map (dtargetVal t) rs

reports, ireports, dreports :: [Summary] -> [String]
ireports tss = zipWith ireport intTargets iss
  where iss = map (istat tss) intTargets
dreports tss = zipWith dreport dblTargets dss
  where dss = map (dstat tss) dblTargets
reports tss = dreports tss ++ ireports tss

ireport :: IntTarget -> Stat -> String
ireport (IntTarget t) (Stat (Mean m,StdDev s,Min mn, Max mx)) = 
  t ++ ": " ++ show m ++ " +/- " ++ show s ++ ", " ++ show mn ++ ", " ++ show mx
dreport :: DblTarget -> Stat -> String
dreport (DblTarget t) (Stat (Mean m,StdDev s,Min mn, Max mx)) = 
  t ++ ": " ++ show m ++ " +/- " ++ show s ++ ", " ++ show mn ++ ", " ++ show mx

-- | select the files to work on. The fileprefix is assumed to be
-- suffixed with integers. We run all files with the same file root prefix.
-- The files will have the directory (first FilePath) prepended.
findSet :: [FilePath]  ->   -- ^ files in the directory
           FilePath    ->   -- ^ directory (will be joined to target files)
           FilePath    ->   -- ^ file prefix
           FileSet
findSet files dir fpref = 
  let bsfiles = map BS.pack files
      bspref  = BS.pack fpref
      bsfs    = filter (\f -> case BS.findSubstring bspref f of 
                                Just _  -> True
                                Nothing -> False) bsfiles
      bsdir   = BS.pack (dir ++ "/")
      bsfsout = map (BS.append bsdir) bsfs 
  in FileSet $ map BS.unpack bsfsout

-- | compute mean and standard deviation of a list of numbers
statistify :: [Double] -> Stat
statistify xs = let m = mean xs
                    mn = Min $ minimum xs
                    mx = Max $ maximum xs
 in Stat (m, stdDev xs m,mn,mx)

reportAll :: String -> FilePath -> IO ()
reportAll s f = do
  BS.writeFile f $ BS.pack s

-- statistics
mean :: [Double] -> Mean
mean xs = Mean $ sum xs / (fromIntegral . length) xs

stdDev :: [Double] -> Mean -> StdDev
stdDev xs (Mean m) = 
  let sumsq = sum $ map (\x -> (x - m)^2) xs
      n     = fromIntegral $ length xs 
  in StdDev . sqrt $ sumsq / (n-1)

-- version
-- $Id$

-- End of file

-- summarizeGC.hs
-- T. M. Kelley
-- Jan 06, 2012
-- (c) Copyright 2012 LANSLLC, all rights reserved

module SummarizeGC -- (findSet
                   -- ,getData
                   -- ,statistify
                   -- ,report
                   -- ,summarizeAll
                   -- ,reportAll
                   -- ,targets
                   -- ,Target
                   -- ) 
  where

import Data.Maybe (fromMaybe)
import Data.List (find)
import Text.Printf
import qualified Data.ByteString.Char8 as BS
import System.Directory
import Debug.Trace

newtype FileSet = FileSet [FilePath]   deriving Show
newtype Target  = Target String        deriving Show
newtype Mean    = Mean Double          deriving Show
newtype StdDev  = StdDev Double        deriving Show
newtype Stat    = Stat (Mean,StdDev)   deriving Show

-- targets serve two masters: the set of things we're looking
-- for, and unique identifiers of interesting lines in the GC
-- statistics output file.
targets = map Target [
           "Total time"
          ,"GC  "
          ,"MUT  "
          ,"Parallel"
          ,"ideal"
          ,"SPARKS"
          ,"converted"
          ,"maximum r"
          ]

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

-- | get the data from the file set for a given target. 
-- TO DO, this should be transposed to get data for 
-- all targets from a single file.
getData :: FileSet -> Target -> IO [Double]
getData (FileSet fs) t = do
  lines <- mapM (findLine t) fs  
  return $ map (parseLine t) lines

-- | compute mean and standard deviation of a list of numbers
statistify :: [Double] -> Stat
statistify xs = let m = mean xs
 in Stat (m, stdDev xs m)

-- reporting
report :: (Target,Stat) -> IO ()
report (targ,Stat (m,s)) = 
  putStrLn $ show targ ++ ": " ++ show m ++ " +- " ++ show s

-- | comma-separated string with <mean>,<stddev> for each target.
-- Useful for importing to spreadsheet... 
summarizeAll :: [Stat] -> String
summarizeAll ss = concat [printf "%f,%f," m s | Stat (Mean m,StdDev s) <- ss ]

reportAll :: String -> FilePath -> IO ()
reportAll s f = do
  BS.writeFile f $ BS.pack s


-- ------------------------------ Helpers ----------------------------
-- reading/parsing helpers

-- get the right line from a file
findLine :: Target -> FilePath -> IO BS.ByteString
findLine (Target st) f = do
  stuff <- BS.readFile f
  let lines = BS.lines stuff
      t     = BS.pack st
  return $ fromMaybe (BS.pack "") $ find (\lin -> case BS.findSubstring t lin of
                                                    Just _  -> True
                                                    Nothing -> False) lines
-- select a parse function
parseLine :: Target -> BS.ByteString -> Double
parseLine (Target t) = 
  case t of
    "Total time" -> parseTotal
    "GC  "       -> parseGC
    "MUT  "      -> parseMUT
    "Parallel"   -> parseGCBal
    "ideal"      -> parseIdeal
    "SPARKS"     -> parseSparks
    "converted"  -> parseConv
    "maximum r"  -> parseRes

parseRes,parseGCBal,parseIdeal,parseTotal,parseMUT,parseGC,parseSparks,parseConv :: BS.ByteString -> Double
parseTotal s  = readIt . strip 's' $ BS.words s !! 4
parseGC s     = readIt . strip 's' $ BS.words s !! 4
parseMUT s    = readIt . strip 's' $ BS.words s !! 4
parseGCBal s  = readIt $ BS.words s !! 4
parseIdeal s  = readIt . strip ')' $ BS.words s !! 9
parseSparks s = readIt $ BS.words s !! 1
parseConv s   = readIt . strip '(' $ BS.words s !! 2
parseRes s    = readIt . strip ',' $ BS.words s !! 0 

readIt = read . BS.unpack

strip :: Char -> BS.ByteString -> BS.ByteString
strip c = BS.concat . BS.split c

-- statistics helpers
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

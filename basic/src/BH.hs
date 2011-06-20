-- BH.hs
-- T. M. Kelley
-- Jun 13, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

{-# LANGUAGE BangPatterns #-}

import Control.Parallel.Strategies
-- import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import System.Environment
import Numerical
-- import System.Exit
import TryNSave
import MC
-- import Physical
import Sphere1D
import Mesh
-- import qualified Data.Vector as V
import Data.List as L
import Source
import PRNG
import Sigma_HBFC
import System.FilePath.Posix (isValid)


runSim :: CLOpts -> IO ()
runSim (CLOpts { nps = n
               , inputF = infile
               , outputF = outfile
               , llimit = ll
               , ulimit = ul
               , chunkSz = chunkSize}
       ) = do
  (clls, lnuer, lnuebarr, lnuxr) <- readMatStateP infile
  let (msh,ndropped) = makeMesh clls ll ul
      mshsz = ncells msh
      lnue    = trim ndropped mshsz lnuer
      lnuebar = trim ndropped mshsz lnuebarr
      lnux    = trim ndropped mshsz lnuxr
      tlly   = runManyParticles n chunkSize msh
  writeTally outfile tlly
  return ()

trim :: Int -> Int -> [a] -> [a]
trim d t l = take t $ drop d l


-- | Perform the simulation for several (at least one) particles
-- in a given mesh.
runManyParticles :: Mesh m => Int -> Int -> m -> Tally
runManyParticles !n !chnkSz msh =
  let
    particles = genParticles n msh testRNG
    tallies   = L.map (runParticle nuE msh) particles
    chunked   = chunk chnkSz tallies
    res       = L.map (L.foldl1' merge) chunked
                `using` parBuffer 10 rdeepseq
  in
    L.foldl1' merge res

-- | Splits a lists into chunks of the given size. TODO: Reuse
-- library functions, or move elsewhere.
chunk :: Int -> [a] -> [[a]]
chunk n = L.unfoldr go
  where
    go xs = case splitAt n xs of
              ([], []) -> Nothing
              r        -> Just r

main :: IO ()
main = do
  argv <- getArgs
  (inopts,nonOpts) <- getOpts argv
  putStrLn $  "opts = " ++ show inopts
  putStrLn $  "ns = " ++ show nonOpts
  opts <- checkOpts inopts
  putStrLn $ "opts checked ok: " ++ show opts
  runSim opts


-- Command line processing 

data CLOpts = CLOpts {
    nps :: Int
  , inputF :: String
  , outputF :: String
  , llimit :: FP
  , ulimit :: FP
  , chunkSz :: Int
  } deriving (Show,Eq)

defaultOpts :: CLOpts
defaultOpts = CLOpts 0 "" "tally" 0 1e12 (-1)

options :: [OptDescr (CLOpts -> CLOpts)]
options = 
  [Option ['n']  ["number-particles"] 
            (ReqArg (\f opts -> opts { nps = read f}) "i")
            "number of particles to run, each species (required)"
  ,Option ['i']  ["input"] 
            (ReqArg (\f opts -> opts {inputF = f}) "FILE")
            "input FILE (required)"
  ,Option ['o']  ["output"] 
            (ReqArg (\f opts -> opts {outputF = f}) "FILE") 
            "output FILE (default \"tally\")"
  ,Option ['l']  ["lower-limit"] 
            (ReqArg (\f opts -> opts { llimit = read f}) "ll") 
            "lower limit in cm"
  ,Option ['u']  ["upper-limit"] 
            (ReqArg (\f opts -> opts { ulimit = read f}) "ul") 
            "upper limit in cm"
  ,Option ['s']  ["chunk-size"] 
            (ReqArg (\f opts -> opts { chunkSz = read f}) "sz") 
            "chunk size (defaults to nps)"
          ]

getOpts :: [String] -> IO (CLOpts,[String])
getOpts argv = 
  case getOpt Permute options argv of
    (o,n,[]) -> return (foldl (flip id) defaultOpts o, n)
    (_,_,es) -> ioError (userError (concat es ++ usageInfo header options))
 
header :: String
header = "Usage: BH [OPTION...] N_Particles Input_File"

checkOptsArgsM :: CLOpts -> Maybe CLOpts
checkOptsArgsM opts = 
  checkNPs opts >>= checkInput >>= checkOutput >>= checkLimits >>= checkChunk

checkOpts :: CLOpts -> IO CLOpts
checkOpts os = case checkOptsArgsM os of 
                 Just opts -> return opts
                 Nothing -> error ("invalid arguments: " ++ show os
                                   ++ "\nexpected:\n" ++ usageInfo header options)

-- To do: instead of checking CL options in Maybe, use a
-- Writer monad that accumulates particular objections. 
checkNPs, checkInput, checkOutput, checkLimits, checkChunk :: CLOpts -> Maybe CLOpts
checkNPs os@(CLOpts {nps = n}) = if n > 0 then Just os else Nothing

-- To do: for files, need better check--this forces us into IO. 
-- | Input file is valid if it exists and user can access it
checkInput os@(CLOpts {inputF = f}) = if isValid f then Just os else Nothing

-- | output file is valid if directory exists and user can write & execute it
checkOutput os@(CLOpts {outputF = f}) = if isValid f then Just os else Nothing

checkLimits os@(CLOpts {llimit = ll, ulimit = ul}) = 
  if ll < ul && ll >= 0.0
  then Just os
  else Nothing

checkChunk os@(CLOpts {nps = n, chunkSz = sz}) = 
  if sz > 0 then Just os else Just os{chunkSz = n}

-- readable :: FilePath -> IO Bool
-- readable f = fileAccess True False False

-- existsAndReadable :: FilePath -> IO Bool
-- existsAndReadable f = 
--   if fileExist f 
--   then Just readable f

-- version
-- $Id$

-- End of file

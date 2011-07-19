-- BH.hs
-- T. M. Kelley
-- Jun 13, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

{-# LANGUAGE BangPatterns #-}

import Control.Parallel.Strategies
import System.Console.GetOpt
import System.Environment
import System.FilePath.Posix (isValid)
import Control.Monad

import Numerical
import TryNSave
import MC
import Physical
import Sphere1D
import Mesh
import Data.List as L
import Source
import PRNG
import Sigma_HBFC

runSim :: CLOpts -> IO ()
runSim opts@(CLOpts { inputF  = infile
                    , outputF = outfile
                    , llimit  = ll
                    , ulimit  = ul
                    }
       ) = do
  -- read input, process into mesh, select corresponding luminosities
  (clls, lnuer, lnuebarr, lnuxr) <- readMatStateP infile
  putStrLn "read material state file"
  let (msh,ndropped)      = mkMesh clls ll ul
      mshsz               = ncells msh
      [lnue,lnuebar,lnux] = map (trim ndropped mshsz) [lnuer,lnuebarr,lnuxr]
  -- run each species
  tallies <- mapM (runOneSpecies msh opts) [(lnue,NuE),(lnuebar,NuEBar),(lnux,NuX)]
  -- write out tallies
  let outfs = map (outfile ++ ) ["_nuE","_nuEbar","_nuX"]
  putStrLn "writing tallies"
  zipWithM_ writeTally outfs tallies

-- | run one neutrino species:
--     1. derive its source statistics (where to put particles)
--     2. print source statistics to stdout
--     3. run particles to get a tally
--     4. print summary of tally
-- TO DO: would be nice to put summarize___ in a writer monad: purify this
runOneSpecies :: Mesh m =>
                 m ->
                 CLOpts ->
                 ([Luminosity],PType) ->
                 IO Tally
runOneSpecies msh
              (CLOpts { nps     = n
                      , chunkSz = chunkSize
                      , simTime = dt
                      , alpha   = a})
              (lnu,nuType) = do
  let statsNu = calcSrcStats lnu dt n
  summarizeStats statsNu nuType
  let tllyNu = runManyParticles statsNu chunkSize msh a
  summarizeTally tllyNu
  return tllyNu

-- | Perform the simulation for several (at least one) particles
-- in a given mesh.
runManyParticles :: Mesh m =>
                    [SrcStat] ->
                    Int ->   -- ^ chunkSize
                    m ->
                    FP ->    -- ^ alpha
                    Tally
runManyParticles stats !chnkSz msh alph =
  let
    particles = genParticlesInCells msh testRNG stats alph
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

trim :: Int -> Int -> [a] -> [a]
trim d t l = take t $ drop d l

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
    nps     :: Int
  , inputF  :: FilePath
  , outputF :: FilePath
  , llimit  :: FP
  , ulimit  :: FP
  , chunkSz :: Int
  , simTime :: Time
  , alpha   :: FP
  } deriving (Show,Eq)

defaultOpts :: CLOpts
defaultOpts = CLOpts 0 "" "tally" 0 1e12 (-1) (Time 1e-7) 2.0

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
  ,Option ['d']  ["dt"]
            (ReqArg (\f opts -> opts { simTime = Time (read f)}) "t")
            "sim time in sec"
  ,Option ['a']  ["alpha"]
            (ReqArg (\f opts -> opts { alpha =  (read f)}) "a")
            "alpha"
          ]


getOpts :: [String] -> IO (CLOpts,[String])
getOpts argv =
  case getOpt Permute options argv of
    (o,n,[]) -> return (foldl (flip id) defaultOpts o, n)
    (_,_,es) -> ioError (userError (concat es ++ usageInfo header options))

header :: String
header = "Usage: BH [OPTION...] N_Particles Input_File"

checkOpts :: CLOpts -> IO CLOpts
checkOpts os = case checkOptsArgsM os of
                 Just opts -> return opts
                 Nothing -> error ("invalid arguments: " ++ show os
                                   ++ "\nexpected:\n" ++ usageInfo header options)

-- To do: instead of checking CL options in Maybe, use a
-- Writer monad that accumulates particular objections.
checkOptsArgsM :: CLOpts -> Maybe CLOpts
checkOptsArgsM opts =
  checkNPs opts >>= checkInput >>= checkOutput >>= checkLimits
             >>= checkChunk >>= checkTime >>= checkAlpha

ensure :: (MonadPlus m) => (a -> Bool) -> a -> m a
ensure p x = guard (p x) >> return x

checkNPs, checkInput, checkOutput, checkLimits :: CLOpts -> Maybe CLOpts
checkChunk, checkTime, checkAlpha :: CLOpts -> Maybe CLOpts

checkNPs    = ensure ((> 0) . nps)

-- To do: for files, need better check--this forces us into IO.
-- | Input file is valid if it exists and user can access it
checkInput  = ensure (isValid . inputF)

-- | output file is valid if directory exists and user can write & execute it
checkOutput = ensure (isValid . outputF)

checkLimits = ensure (\ CLOpts {llimit = ll, ulimit = ul} -> ll < ul && ll >= 0)

checkTime   = ensure (\ CLOpts {simTime = Time dt} -> dt > 0)

checkAlpha  = ensure ((> 0) . alpha)

-- | Initialize chunk size to the whole stream unless explicitly given.
checkChunk os@(CLOpts {nps = n, chunkSz = sz}) =
  return (if sz > 0 then os else os {chunkSz = n})

-- version
-- $Id$

-- End of file

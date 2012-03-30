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
import qualified Data.Vector as V


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
import FileInputCF

type VecLums = V.Vector Luminosity

runSim :: CLOpts -> IO ()
runSim opts@(CLOpts { inputF  = infile
                    , outputF = outfile
                    , llimit  = ll
                    , ulimit  = ul
                    , rngType = rngType
                    , seed    = sd
                    , nps     = n
                    , chunkSz = chunkSize
                    }
       ) = do
  -- read input, process into mesh, select corresponding luminosities,
  -- initialize rngs
  (clls, lnuer, lnuebarr, lnuxr) <- parseFile infile
  putStrLn "read material state file"
  let (msh, !ndropped)    = mkMesh clls ll ul
      !mshsz              = ncells msh
      [lnue,lnuebar,lnux] = map (trim ndropped mshsz) [lnuer,lnuebarr,lnuxr]
  let !n' = (n `div` chunkSize) + 1
  let ![g1, g2, g3] = createGens rngType sd 3 n'
  putStrLn $ "ndropped: " ++ show ndropped ++ ", mesh size: " ++ show mshsz
  -- run each species
  let runs = [(lnue,NuE,nuE,g1),(lnuebar,NuEBar,nuEBar,g2),(lnux,NuX,nuX,g3)]
  tallies <- mapM (runOneSpecies msh opts) runs
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
                 m                               ->
                 CLOpts                          ->
                 (VecLums,PType,Lepton,IO [RNG]) ->
                 IO Tally
runOneSpecies msh
              (CLOpts { nps     = n
                      , chunkSz = chunkSize
                      , simTime = dt
                      , alpha   = a})
              (lnu,nuType,lep,gens') = do
  let statsNu = calcSrcStats lnu dt n
  summarizeStats statsNu nuType
  gens <- gens'
  let tllyNu = runManyParticles statsNu chunkSize msh gens lep a
  summarizeTally tllyNu
  return tllyNu

-- | Perform the simulation for several (at least one) particles
-- in a given mesh.
runManyParticles :: Mesh m =>
                    [SrcStat] ->
                    Int       ->    -- ^ chunkSize
                    m         ->
                    [RNG]     ->
                    Lepton    ->
                    FP        ->    -- ^ alpha
                    Tally
runManyParticles stats !chnkSz msh gens lep alph =
  let
    particles :: [Rnd Particle]
    particles = genParticlesInCells stats msh alph
    tallies :: [Rnd (Tally -> Tally)]
    tallies   = L.map (>>= runParticle lep msh) particles
    chunked :: [[Rnd (Tally -> Tally)]]
    chunked   = chunk chnkSz tallies
{-
    premerged :: [Rnd Tally]
    premerged = L.map (liftM (foldl' mappend mempty) . sequence) chunked
    combd :: [Tally]
    combd     = L.zipWith (\ x g -> runRnd g x) premerged gens
    res       = combd `using` parBuffer 10 rdeepseq
-}
    res = L.zipWith (\ xs g -> procTallies g (emptyTally msh) xs) chunked gens
            `using` parBuffer 10 rdeepseq
  in
    foldl' merge (emptyTally msh) res

procTallies :: RNG -> Tally -> [Rnd (Tally -> Tally)] -> Tally
procTallies _   t []           = t
procTallies rng t (Rnd x : xs) = x (\ r rng' -> let t' = r t
                                                in  t' `seq` procTallies rng' t' xs) rng

createGens :: RNGType -> Int -> Int -> Int -> [IO [RNG]]
createGens rngType sd k n = do
    -- The following construction is an a bit clumsy attempt to prevent
    -- space leaks between the runs.
    map (\ i -> do
                  -- We assume that create is essentially pure
                  rngs <- create rngType sd (i * n)
                  return (drop ((i - 1) * n) rngs)
        ) [1 .. k]
  where
    create :: RNGType -> Int -> Int -> IO [RNG]
    create StdGen = createStdGen
    create LFG    = createLFG

-- | Splits a lists into chunks of the given size. TODO: Reuse
-- library functions, or move elsewhere.
chunk :: Int -> [a] -> [[a]]
chunk n = L.unfoldr go
  where
    go xs = case splitAt n xs of
              ([], _) -> Nothing
              r       -> Just r

trim :: Int -> Int -> V.Vector a -> V.Vector a
trim d t l = V.take t $ V.drop d l

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
    nps     :: !Int
  , inputF  :: !FilePath
  , outputF :: !FilePath
  , llimit  :: !FP
  , ulimit  :: !FP
  , chunkSz :: !Int
  , simTime :: !Time
  , alpha   :: !FP
  , rngType :: !RNGType
  , seed    :: !Int
  , help    :: !Bool
  } deriving (Show, Eq)

data RNGType = StdGen | LFG
  deriving (Show, Eq, Read)

defaultOpts :: CLOpts
defaultOpts = CLOpts 0 "" "tally" 0 1e12 (-1) (Time 1e-7) 2.0 LFG 42 False

options :: [OptDescr (CLOpts -> CLOpts)]
options =
  [Option ['n']  ["number-particles"]
            (ReqArg (\f opts -> opts { nps = read f }) "i")
            "number of particles to run, each species (required)"
  ,Option ['i']  ["input"]
            (ReqArg (\f opts -> opts {inputF = f }) "FILE")
            "input FILE (required)"
  ,Option ['o']  ["output"]
            (ReqArg (\f opts -> opts {outputF = f }) "FILE")
            "output FILE (default \"tally\")"
  ,Option ['l']  ["lower-limit"]
            (ReqArg (\f opts -> opts { llimit = read f }) "ll")
            "lower limit in cm"
  ,Option ['u']  ["upper-limit"]
            (ReqArg (\f opts -> opts { ulimit = read f }) "ul")
            "upper limit in cm"
  ,Option ['c']  ["chunk-size"] 
            (ReqArg (\f opts -> opts { chunkSz = read f}) "sz") 
            "chunk size (defaults to nps)"
  ,Option ['d']  ["dt"]
            (ReqArg (\f opts -> opts { simTime = Time (read f) }) "t")
            "sim time in sec"
  ,Option ['a']  ["alpha"]
            (ReqArg (\f opts -> opts { alpha = read f }) "a")
            "alpha"
  ,Option ['R']  ["rng-type"]
            (ReqArg (\f opts -> opts { rngType = read f }) "R")
            "type of rng to use (lfg, stdgen)"
  ,Option ['s']  ["rng-seed"] 
            (ReqArg (\f opts -> opts { seed = read f}) "s")
            "seed"
  ,Option ['h']  ["help"] 
            (NoArg (\opts -> opts { help = True})) 
            "print useful help message and exit"
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
                 Nothing -> error ("some argument(s) invalid: " ++ show os
                                   ++ "\n" ++ usageInfo header options)

-- To do: instead of checking CL options in Maybe, use a
-- Writer monad that accumulates particular objections.
checkOptsArgsM :: CLOpts -> Maybe CLOpts
checkOptsArgsM opts =
  checkNPs opts >>= checkInput >>= checkOutput >>= checkLimits
             >>= checkChunk >>= checkTime >>= checkAlpha >>= checkHelp

ensure :: (MonadPlus m) => (a -> Bool) -> a -> m a
ensure p x = guard (p x) >> return x

checkNPs, checkInput, checkOutput, checkLimits :: CLOpts -> Maybe CLOpts
checkChunk, checkTime, checkAlpha, checkHelp :: CLOpts -> Maybe CLOpts

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

checkHelp   = ensure (not . help)

-- version
-- $Id$

-- End of file

-- BH.hs
-- T. M. Kelley
-- Jun 13, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

{-# LANGUAGE BangPatterns, TupleSections, FlexibleInstances #-}

-- This differs from BH only in that it uses an LFG random number generator,
-- instead of the standard LCG in System.Random

import Control.DeepSeq (deepseq,rnf,NFData)
import System.Console.GetOpt
import System.Environment
import System.FilePath.Posix (isValid)
import Control.Monad (MonadPlus,guard,liftM)
import Data.Word
import qualified Data.Vector as V

import Numerical
import TryNSave
import MC
import Physical
import Sphere1D
import Mesh
import Data.List as L
import Source
import Philo2
import Sigma_HBFC
import FileInputCF
import RunParticles
import Partition (renumberStats)
import MPISwitch as MPI 
import Histogram

type VecLums = V.Vector Luminosity
type Key = Philo4x32Key

instance NFData a => NFData (V.Vector a) where
  rnf v = V.foldl' (\x y -> y `deepseq` x) () v
instance NFData Luminosity
instance NFData Cell 

runSim :: CLOpts -> IO ()
runSim opts@(CLOpts { inputF  = infile
                    , outputF = outfile
                    , llimit  = ll
                    , ulimit  = ul
                    , seed    = sd
                    , nps     = nIn
                    , chunkSz = chunkSize
                    , alpha   = a
                    , simTime = dt
                    }
       ) = do
  MPI.init
  rank   <- MPI.commRank MPI.commWorld
  commSz <- liftM fromIntegral $ MPI.commSize MPI.commWorld
  time0  <- MPI.wtime

  -- read input, process into mesh, select corresponding luminosities,
  -- initialize rngs
  (clls, lnuer, lnuebarr, lnuxr) <- time0 `deepseq` parseFile infile
  putStrLn "read material state file"
  let (!msh,!ndropped)       = clls `deepseq` mkMesh clls ll ul
      !mshsz                 = ncells msh
      [!lnue,!lnuebar,!lnux] = map (trim ndropped mshsz) [lnuer,lnuebarr,lnuxr]

  time1 <- MPI.wtime

  let !nc = time1 `deepseq` (nIn `div` chunkSize) + 1
      -- !keys1 = mkKeys (Seed sd) nc (Offset $        1)
      -- !keys2 = mkKeys (Seed sd) nc (Offset $   nc + 1)
      -- !keys3 = mkKeys (Seed sd) nc (Offset $ 2*nc + 1)
      key = Key4x32 sd 0

  putStrLn $ "ndropped: "    ++ show ndropped ++ 
             ", mesh size: " ++ show mshsz ++ 
             ", n_chunks: "  ++ show nc

  -- run each species
  let fc = runParticlesParList     --parList
      -- fc = runParticlesParBuffer  -- Par monad
      -- fc = runParticlesPar        -- parBuffer
      fcore = fc chunkSize msh a (MPI.fromRank rank,fromIntegral commSz)
      f1 = fcore nuE key
      f2 = fcore nuEBar key 
      f3 = fcore nuX key

  talliesNuE    <- runOneSpeciesWith nIn dt (rank,commSz) (lnue,NuE) f1
  talliesNuEBar <- runOneSpeciesWith nIn dt (rank,commSz) (lnuebar,NuEBar) f2
  talliesNuX    <- runOneSpeciesWith nIn dt (rank,commSz) (lnux,NuX) f3

  time2 <- talliesNuE    `deepseq` 
           talliesNuEBar `deepseq` 
           talliesNuX    `deepseq` 
           MPI.wtime

  if rank /= MPI.toRank 0
  then MPI.sendTally talliesNuE
       >> MPI.sendTally talliesNuEBar
       >> MPI.sendTally talliesNuX
  else do
    tallyNuE    <- MPI.recvTally talliesNuE
    tallyNuEBar <- MPI.recvTally talliesNuEBar
    tallyNuX    <- MPI.recvTally talliesNuX
    writeTally (outfile++"_nuE")    tallyNuE
    writeTally (outfile++"_nuEBar") tallyNuEBar
    writeTally (outfile++"_nuX")    tallyNuX
    -- histogram escape events
    -- let bounds = [Energy i | i <- [0..100]]
    --     histNuE    = foldr count (mkHist bounds) $ escape tallyNuE
    --     histNuEBar = foldr count (mkHist bounds) $ escape tallyNuEBar
    --     histNuX    = foldr count (mkHist bounds) $ escape tallyNuX
    -- writeHistogram ("hist_"++outfile++"_nuE")    histNuE
    -- writeHistogram ("hist_"++outfile++"_nuEBar") histNuEBar
    -- writeHistogram ("hist_"++outfile++"_nuX")    histNuX
  

  time3 <- MPI.wtime

  putStrLn $ "Rank " ++ show rank 
             ++ ", interval 1: " ++ show (MPI.diffTime time1 time0)
             ++ ", interval 2: " ++ show (MPI.diffTime time2 time1)
             ++ ", interval 3: " ++ show (MPI.diffTime time3 time2)

  MPI.finalize

-- | run one neutrino species: derive its source statistics (where to put
--  particles) and run them to get a tally
runOneSpeciesWith :: Word32               ->  -- ^ number particles
                     Time                 -> 
                    (MPI.Rank,Word32)     ->  -- ^ rank,nranks
                    (VecLums,PType)       ->
                    (SrcStats -> SrcStats -> Tally)  ->
                    IO Tally
runOneSpeciesWith nps dt (rank,commSz) (lnu,nuType) f = do
  let statsNuRaw = calcSrcStats lnu dt nps
      statsNu    = renumberStats rank commSz statsNuRaw
  summarizeStats statsNu nuType
  summarizeStats statsNuRaw nuType
  let tllyNu = f statsNuRaw statsNu
  return tllyNu

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

--                 ***   Command line processing   ***

data CLOpts = CLOpts {
    nps     :: Word32
  , inputF  :: FilePath
  , outputF :: FilePath
  , llimit  :: FP
  , ulimit  :: FP
  , chunkSz :: Word32
  , simTime :: Time
  , alpha   :: FP
  , seed    :: Word32
  , help    :: Bool
  } deriving (Show,Eq)

defaultOpts :: CLOpts
defaultOpts = CLOpts 0 "" "tally" 0 1e12 (-1) (Time 1e-7) 2.0 42 False

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
  ,Option ['s']  ["rng--seed"] 
            (ReqArg (\f opts -> opts { seed =  (read f)}) "s") 
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
header = "Usage: BH [OPTION...] -n <n_particles> -i <input_file>"

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

checkHelp   = ensure (not . help)

-- version
-- $Id$

-- End of file

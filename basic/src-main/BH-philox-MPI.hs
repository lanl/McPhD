-- BH.hs
-- T. M. Kelley
-- Jun 13, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

{-# LANGUAGE BangPatterns, TupleSections #-}

-- This differs from BH only in that it uses an LFG random number generator,
-- instead of the standard LCG in System.Random

import Control.Parallel.Strategies
import Control.Parallel (par,pseq)
import Control.DeepSeq (deepseq)
import System.Console.GetOpt
import System.Environment
import System.FilePath.Posix (isValid)
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Word
import qualified Data.Serialize as Serialize
import qualified Data.Vector as V
import Control.Parallel.MPI.Simple
import qualified Control.Parallel.MPI.Base as MPIB
import qualified Control.Parallel.MPI.Simple as MPI

import Debug.Trace

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

type VecLums = V.Vector Luminosity
type Key = Philo4x32Key

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
  -- init MPI
  MPI.init
  rank   <- MPI.commRank MPI.commWorld
  commSz <- liftM fromIntegral $ MPI.commSize MPI.commWorld
  time0  <- MPI.wtime

  -- read input, process into mesh, select corresponding luminosities,
  -- initialize rngs
  (clls, lnuer, lnuebarr, lnuxr) <- time0 `deepseq` parseFile infile
  putStrLn "read material state file"
  let (msh,!ndropped)      = mkMesh clls ll ul
      !mshsz               = ncells msh
      [lnue,lnuebar,lnux] = map (trim ndropped mshsz) [lnuer,lnuebarr,lnuxr]

  time1 <- MPI.wtime

  -- Offset RNG keys by rank times number of chunks per rank.
  -- In the case commSz -> 1, we should get base -> 0, ensuring the
  -- same results as if we ran serial.
  let n   = time1 `deepseq`
            nIn `div` fromIntegral commSz
      !nc = (nIn `div` chunkSize) + 1
      !keys1 = mkKeys (Seed sd) nc (Offset $        1)
      !keys2 = mkKeys (Seed sd) nc (Offset $   nc + 1)
      !keys3 = mkKeys (Seed sd) nc (Offset $ 2*nc + 1)

  putStrLn $ "ndropped: "    ++ show ndropped ++
             ", mesh size: " ++ show mshsz    ++
             ", n_chunks: "  ++ show nc

  -- run each species
  -- select a function that runs particles
  let fc = runParticlesParList   -- parList
           -- runParticlesPar       -- Par monad
           -- runParticlesParBuffer -- parBuffer
      fcore = fc chunkSize msh a (fromRank rank,fromIntegral commSz)
      f1 = fcore nuE    keys1
      f2 = fcore nuEBar keys2
      f3 = fcore nuX    keys3

  talliesNuE    <- runOneSpeciesWith nIn dt (rank,commSz) (lnue,NuE) f1
  talliesNuEBar <- runOneSpeciesWith nIn dt (rank,commSz) (lnuebar,NuEBar) f2
  talliesNuX    <- runOneSpeciesWith nIn dt (rank,commSz) (lnux,NuX) f3

  time2 <- talliesNuE    `deepseq`
           talliesNuEBar `deepseq`
           talliesNuX    `deepseq`
           MPI.wtime

  if rank /= 0
  then sendTally talliesNuE
       >> sendTally talliesNuEBar
       >> sendTally talliesNuX
  else do
    tallyNuE    <- recvTally talliesNuE
    tallyNuEBar <- recvTally talliesNuEBar
    tallyNuX    <- recvTally talliesNuX
    writeTally (outfile++"_nuE")    tallyNuE
    writeTally (outfile++"_nuEBar") tallyNuEBar
    writeTally (outfile++"_nuX")    tallyNuX

  time3 <- MPI.wtime

  putStrLn $ "Rank " ++ show rank ++ ", interval 1: " ++ show (time1 - time0)
                ++ ", interval 2: " ++ show (time2 - time1)
                ++ ", interval 3: " ++ show (time3 - time2)

  MPI.finalize

-- | run one neutrino species: derive its source statistics (where to put
--  particles) and run them to get a tally
runOneSpeciesWith :: Word32              ->  -- ^ number particles
                     Time                ->
                     (Rank,Word32)       ->  -- ^ rank,nranks
                    (VecLums,PType)      ->
                    (SrcStats -> SrcStats -> Tally)  ->
                    IO Tally
runOneSpeciesWith nps dt (rank,commSz) (lnu,nuType) f = do
  let statsNuRaw = calcSrcStats lnu dt nps
      statsNu    = renumberStats rank commSz statsNuRaw
  summarizeStats statsNu nuType
  summarizeStats statsNuRaw nuType
  let tllyNu = f statsNuRaw statsNu
  return tllyNu

sendTally :: Tally -> IO ()
sendTally t = MPI.gatherSend commWorld 0 (Serialize.encode t)

recvTally :: Tally -> IO Tally
recvTally t = do
  msgs <- MPI.gatherRecv commWorld 0 (Serialize.encode t)
  let ts = map (forceEither . Serialize.decode) msgs
  return $ L.foldl' mappend mempty ts

-- Cribbed from MissingH
forceEither :: Show e => Either e a -> a
forceEither (Left x) = error (show x)
forceEither (Right x) = x

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
  -- everybody runs, gather all tallies back to the master (rank 0).
  runSim opts



--                    ***  Command line processing   ***

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

-- End of file

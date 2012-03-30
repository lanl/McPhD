-- testChunks.hs
-- T. M. Kelley
-- Jan 06, 2012
-- (c) Copyright 2012 LANSLLC, all rights reserved

{-| test the performance of black-hole as we vary the number of threads. -}

import SummarizeGC

import Text.Printf
import System.Directory
import System.Console.GetOpt
import System.Environment
import System.Process
import System.Random
import Data.Word (Word32)

gcFilePat  = "%s/N_%i_gc_%i"
runFilePat = "%s/N_%i__run_%i"

runARun :: FilePath ->  -- directory for outputs
           String ->    -- basic command line
           Int ->       -- number of runs
           Int ->       -- number of ranks
           IO ()
runARun _ _ 0 _ = return ()
runARun rundir commStr n nRanks = do
  -- pick a seed
  sd <- randomIO :: IO Word32
  let gcFile, runFile, comm :: String
      gcFile  = printf  gcFilePat rundir nRanks n
      runFile = printf runFilePat rundir nRanks n
      comm    = printf commStr nRanks sd gcFile runFile
  putStrLn $ "now running: " ++ comm
  system comm >> runARun rundir commStr (n-1) nRanks

printInt :: Int -> String
printInt = printf "%d"

--
-- run for one value
--
runOneVal :: FilePath -> -- directory for output files
             Bool     ->  -- summarize only
             Int ->  -- number of runs
             Int ->  -- number of ranks
             IO ()
runOneVal dir summOnly nRuns nRanks = do
  -- cl to run
  let commStr = "mpirun -n %i ./black-hole-mpi -i ../../data/p.7 -n 10000000 -c 25000 -s %i +RTS -s%s -N1 -RTS 2>&1 | tee %s"

  -- execute all the runs
  if not summOnly
  then runARun dir commStr nRuns nRanks
  else runARun dir commStr 0 nRanks  -- how does one say "pass"?

  -- digest the output
  -- get file set
  files <- getDirectoryContents dir
  -- compute data
  -- for -N1, skip Parallel GC work balance--it doesn't parse
  let fPrefix = printf "N_%i_gc_" nRanks
      fset    = findSet files dir fPrefix
      targSet = filter (\ (Target t) -> t /= "Parallel") targets
      -- targSet = if nRanks == 1
      --           then filter (\ (Target t) -> t /= "Parallel") targets
      --           else targets
  dataz <- mapM (getData fset) targSet
  let statz = map statistify dataz
  -- report
  mapM report $ zip targSet statz
  let summary = summarizeAll statz  
      summPref = dir ++ "/summ_n__" ++ printInt nRanks ++ "__" ++ printInt nRuns ++ "_runs"
  reportAll summary summPref


--
-- main
--
main = do
  -- cl options
  argv <- getArgs
  (opts,nonOpts) <- getOpts argv
  let dir       = cldir opts
      summOnly  = clSummarizeOnly opts
      nRuns     = 10
  
  let rankCts = if (clrunAll opts)
                then [1..(clnRanks opts)]
                else [clnRanks opts]
  mapM (runOneVal dir summOnly nRuns) rankCts

--
-- command line processing
--
data CLOpts = CLOpts {
    cldir           :: FilePath
  , clnRanks        :: Int
  , clSummarizeOnly :: Bool
  , clrunAll        :: Bool
  } deriving (Show, Eq)

defaultOpts :: CLOpts
defaultOpts = CLOpts "N-strong-scaling" 1 False False

options :: [OptDescr (CLOpts -> CLOpts)]
options =
  [ Option ['d'] ["directory"]
           (ReqArg (\f opts -> opts {cldir = f}) "d")
           "directory files are in"
  , Option ['n'] ["number-threads"]
           (ReqArg (\f opts -> opts {clnRanks = read f}) "n")
           "number of ranks"
  , Option ['s'] ["summarize"]
           (NoArg (\opts -> opts {clSummarizeOnly = True}))
           "summarize an existing run--i.e. don't re-run"
  , Option ['a'] ["run-all"]
           (NoArg (\opts -> opts {clrunAll = True}))
           "run an ensemble of thread counts, from 1 to argument of -n"
  ]


getOpts :: [String] -> IO (CLOpts,[String])
getOpts argv =
  case getOpt Permute options argv of
    (o,n,[]) -> return (foldl (flip id) defaultOpts o, n)
    (_,_,es) -> ioError (userError (concat es ++ usageInfo header options))


header :: String
header = "Usage: summGC [OPTION...]"


-- version
-- $Id$

-- End of file

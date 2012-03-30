-- testChunks.hs
-- T. M. Kelley
-- Jan 06, 2012
-- (c) Copyright 2012 LANSLLC, all rights reserved

{-| test the performance of black-hole as we vary the number of threads. -}

import qualified SummarizeGC as GC
import qualified SummarizeBH as BH
import qualified ParseBH as PBH
import Text.Printf
import System.Directory
import System.Console.GetOpt
import System.Environment
import System.Process
import System.Random
import Debug.Trace

runARun :: FilePath ->  -- directory for outputs
           String ->    -- basic command line
           Int ->       -- number of runs
           Int ->       -- number of cores
           IO ()
runARun _ _ 0 _ = return ()
runARun rundir commStr n nCores = do
  -- pick a seed
  sd <- randomIO :: IO Int
  let comm = printf commStr sd rundir nCores n nCores rundir nCores n
  putStrLn $ "now running: " ++ comm
  system comm >> runARun rundir commStr (n-1) nCores

printInt :: Int -> String
printInt = printf "%d"

--
-- run for one value
--
runOneVal :: FilePath -> -- directory for output files
             Bool     ->  -- summarize only
             Int ->  -- number of runs
             Int ->  -- number of cores
             IO ()
runOneVal dir summOnly nRuns nCores = do
  -- cl to run
  let commStr = "time ./black-hole -i ../../data/p.7 -n 1000000 -c 50000 -s %i +RTS -s%s/s__gc_N%i_%i -N%i -RTS 2>&1 | tee %s/s__N%i_run_%i"

  -- execute all the runs
  if not summOnly
  then runARun dir commStr nRuns nCores
  else runARun dir commStr 0 nCores  -- how does one say "pass"?

  -- digest the output
  -- get file set
  files <- getDirectoryContents dir
  -- compute data
  -- for -N1, skip Parallel GC work balance--it doesn't parse
  let fPrefix = printf "s__gc_N%i_" nCores 
      fRunPrf = printf "s__N%i_run_" nCores 
  let fset    = GC.findSet files dir fPrefix
      targSet = if nCores == 1
                then filter (\ (GC.Target t) -> t /= "Parallel") GC.targets
                else GC.targets
  dataz <- mapM (GC.getData fset) targSet
  let statz = map GC.statistify dataz
  -- report
  mapM GC.report $ zip targSet statz
  let summary = GC.summarizeAll statz  
      summPref = dir ++ "/summ_n__" ++ printInt nCores ++ "__" ++ printInt nRuns ++ "_runs"
  GC.reportAll summary summPref >> reportMCData files dir fRunPrf 

reportMCData :: [FilePath] ->  -- ^ output files
                FilePath   ->  -- ^ directory
                FilePath   ->  -- ^ file prefix
                IO ()
reportMCData files dr prf = do
  fconts <- PBH.readSet $ BH.findSet files dr prf
  let tss = PBH.summSet fconts
      nuEreps  = BH.reports $ PBH.nuSumms PBH.NuE tss
      nuEbreps = BH.reports $ PBH.nuSumms PBH.NuEBar tss
      nuXreps  = BH.reports $ PBH.nuSumms PBH.NuX tss
      fname  = dr ++ "/summ_seed"
      outstr = ("For NuE:" : nuEreps) ++
               ("For NuEBar:" : nuEbreps) ++
               ("For NuX:" : nuXreps)
  writeFile fname (unlines outstr)
      
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
      nCores    = clnCores opts
      nSeeds    = clnSeeds opts

  runOneVal dir summOnly nRuns nCores

--
-- command line processing
--
data CLOpts = CLOpts {
    cldir           :: FilePath
  , clnCores        :: Int
  , clnSeeds        :: Int
  , clSummarizeOnly :: Bool
  , clrunAll        :: Bool
  } deriving (Show, Eq)

defaultOpts :: CLOpts
defaultOpts = CLOpts "N-strong-scaling" 1 10 False False

options :: [OptDescr (CLOpts -> CLOpts)]
options =
  [ Option ['d'] ["directory"]
           (ReqArg (\f opts -> opts {cldir = f}) "d")
           "directory files are in"
  , Option ['n'] ["number-threads"]
           (ReqArg (\f opts -> opts {clnCores = read f}) "n")
           "number of cores"
  , Option ['m'] ["number-seeds"]
           (ReqArg (\f opts -> opts {clnSeeds = read f}) "m")
           "number of seeds to try"
  , Option ['s'] ["summarize"]
           (NoArg (\opts -> opts {clSummarizeOnly = True}))
           "summarize an existing run--i.e. don't re-run"
  -- , Option ['a'] ["run-all"]
  --          (NoArg (\opts -> opts {clrunAll = True}))
  --          "run an ensemble of thread counts, from 1 to argument of -n"
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

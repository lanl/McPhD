-- testChunks.hs
-- T. M. Kelley
-- Jan 06, 2012
-- (c) Copyright 2012 LANSLLC, all rights reserved

{-| test the performance of black-hole as we vary the chunk size parameter. -}

import SummarizeGC

import Text.Printf
import System.Directory
import System.Console.GetOpt
import System.Environment
import System.Process

defaultChunks = [30,50,100,300,500,1000,3000,5000,10000,30000,50000,100000]

runARun :: FilePath -> String -> Int -> Int -> IO ()
runARun _ _ 0 _ = return ()
runARun rundir commStr n chunkSize = do
  let comm = printf commStr rundir chunkSize n chunkSize rundir chunkSize n
  putStrLn $ "now running: " ++ comm
  system comm >> runARun rundir commStr (n-1) chunkSize

printInt :: Int -> String
printInt = printf "%d"

-- perform runs for a single value of the parameter
runOneVal :: FilePath -> -- directory for output files
             Bool     ->  -- summarize only
             Int ->  -- number of runs
             Int ->  -- chunk size
             IO ()
runOneVal dir summOnly nRuns chunkSize = do

  -- cl to run
  let commStr = "time ./black-hole -i ../../data/p.7 -n 1000000 -s 4523645 +RTS -s%s/c_%i_gc_%i -N6 -RTS -c %i 2>&1 | tee %s/c_%i_%i"

  -- execute all the runs
  if not summOnly
  then runARun dir commStr nRuns chunkSize
  else runARun dir commStr 0     chunkSize  -- how does one say "pass"?

  -- digest the output
  -- get file set
  files <- getDirectoryContents dir
  -- compute data
  let fPrefix = printf "c_%i_gc_" chunkSize
      fset    = findSet files dir fPrefix
  dataz <- mapM (getData fset) targets
  let statz = map statistify dataz
  -- report
  mapM report $ zip targets statz
  let summary  = summarizeAll statz  
      summPref = dir ++ "/summ_c__" ++ printInt chunkSize ++ "__" ++ printInt nRuns ++ "_runs"
  reportAll summary summPref


--
-- main
--
main = do
  -- cl options
  argv <- getArgs
  (opts,nonOpts) <- getOpts argv
  let dir      = cldir opts
      summOnly = clSummarizeOnly opts
      nRuns    = 10
  let cSizes = if (clrunAll opts)
               then defaultChunks
               else [clchunkSize opts]
      runStr = "will run the following chunk size" ++ if length cSizes > 1 
                                                      then "s" 
                                                      else ""
  putStrLn $ runStr ++ ": " ++ show cSizes

  mapM (runOneVal dir summOnly nRuns) cSizes


--
-- command line processing
--
data CLOpts = CLOpts {
    cldir       :: FilePath
  , clchunkSize :: Int
  , clSummarizeOnly :: Bool
  , clrunAll    :: Bool
  } deriving (Show, Eq)

defaultOpts :: CLOpts
defaultOpts = CLOpts "." 1 False False

options :: [OptDescr (CLOpts -> CLOpts)]
options =
  [ Option ['d'] ["directory"]
           (ReqArg (\f opts -> opts {cldir = f}) "d")
           "directory files are in"
  , Option ['c'] ["chunk-size"]
           (ReqArg (\f opts -> opts {clchunkSize = read f}) "c")
           "chunk size"
  , Option ['s'] ["summarize"]
           (NoArg (\opts -> opts {clSummarizeOnly = True}))
           "summarize an existing run--i.e. don't re-run"
  , Option['a']["run-all"]
            (NoArg (\opts -> opts {clrunAll = True}))
           "run an ensemble of chunk sizes"

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

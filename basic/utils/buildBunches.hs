-- testChunks.hs
-- T. M. Kelley
-- Jan 06, 2012
-- (c) Copyright 2012 LANSLLC, all rights reserved

{-| test the performance of black-hole as we vary the number of threads. -}

import Text.Printf
import System.Directory
import System.Console.GetOpt
import System.Environment
import System.Process (system)
import System.Exit (ExitCode(..))

configs = 
  [" -O2 --builddir=./build/O2"
  ," -O2 --builddir=./build/mpi-threaded-O2 -fmpi --ghc-options=\"-threaded\""
  ," -O2 --builddir=./build/threaded-O2 --ghc-options=\"-threaded\""
  ," -O2 --builddir=./build/log-O2 --ghc-options=\"-threaded -eventlog\""
  ," -O2 --builddir=./build/prof-O2 --ghc-options=\"-prof -auto-all -caf-all\""
  ]

builds = ["./build/O2",
          "./build/mpi-threaded-O2",
          "./build/threaded-O2",
          "./build/log-O2",
          "./build/prof-O2"]

runConfig :: String -> IO ()
runConfig c = do
  let base = "cabal configure %s"
      comm = printf base c
  putStrLn $ "Now running: " ++ comm
  rval <- system comm
  case rval of 
    ExitSuccess -> return ()
    ExitFailure i -> error $ "configuration " ++ comm ++ " failed! return value = " ++ show i

runBuild :: String -> IO ()
runBuild b = do
  let base = "cabal build --builddir=%s"
      comm = printf base b
  putStrLn $ "Now running: " ++ comm
  rval <- system comm
  case rval of 
    ExitSuccess -> return ()
    ExitFailure i -> error $ "build " ++ comm ++ " failed! return value = " ++ show i

main = do
  argv <- getArgs
  (opts,nonOpts) <- getOpts argv
  if clConfig opts
  then mapM runConfig configs >> mapM runBuild builds
  else mapM runBuild builds


--
-- command line processing
--
data CLOpts = CLOpts {
    clConfig :: Bool
  } deriving (Show, Eq)

defaultOpts :: CLOpts
defaultOpts = CLOpts False

options :: [OptDescr (CLOpts -> CLOpts)]
options =
  [ Option ['c'] ["configure"]
           (NoArg (\opts -> opts {clConfig = True}))
           "run configure steps first"
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

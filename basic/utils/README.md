Utilities for basic
===========================

This directory contains several utility scripts for benchmarking performance of black-hole with respect to one command line parameter, using GHC's GC statistics. Each script uses the module SummarizeGC, a homely but effective script for averaging GC statistics from multiple runs.

Available tests
---------------
test-seed: test the effect of running with a different seed every time. Interesting--suggests we're quire poorly resolved.

testChunks: run black-hole 10 times with a given chunk size (-c chksz). TO DO: optionally work on an ensemble of chunk sizes.

testN: run black-hole 10 times with a given number of threads (+RTS -N i).

testN-mpi-strong: Run with different values of N (number of MPI ranks), keeping the number of particles fixed.

testN-weak: run black-hole 10 times for each member of on an ensemble of thread counts (1 to N), scaling the work load so that the number of particles per thread is constant.

Build them using "for f in test*hs ; do ghc -O2 ${f} ; done".

Build many configurations of 'basic'
------------------------------------

To compare various configuration and build options for the 'basic' library and applications, a tool was written to automate building multiple configurations. "buildBunches" builds multiple configurations of the basic library and the BH-philox driver applications. Configurations include O2, O2 prof, O2 threaded, O2 threaded eventlog in "./build".
Run buildBunches with -c to configure all these options.

Build the buildBunches tool itself via "ghc -O2 buildBunches.hs"

Run it from the top level directory (up one level from here).

Assumed directory structure
---------------------------
The testN* scripts assume that they are in a particular place relative to the data/ directory, namely that data/  is two levels higher. Here's how my directory layout looks:

|- src
|- src-main
|- data
|- build
    |- O2
    |- threaded-O2
    |- etc
|-runs
    |- O2
    |- threaded-O2
         |- running test<blah> here
              |- chunk
              |- N-strong
              |- N-weak
    |- etc
|- etc

Feel free to modify to suit your needs. I typically sym link the executable into the run directory.


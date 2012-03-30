This directory contains several utility scripts for benchmarking performance of black-hole with respect to one command line parameter, using GHC's GC statistics. 

Each script uses the module SummarizeGC, a homely but effective script for averaging GC statistics from multiple runs.

testChunks: run black-hole 10 times with a given chunk size (-c <i>). TO DO: optionally work on an ensemble of chunk sizes.

testN: run black-hole 10 times with a given number of threads (+RTS -N<i>).

testN-weak: run black-hole 10 times for each member of on an ensemble of thread counts (1-n), scaling the work load so that the number of particles per thread is constant.

The scripts have a command line with dummy parameters that are filled in via printf. The command lines assume they're being run from a directory with a specific relation to the 'data' directory: they assume the data directory is two levels higher. Here's how my directory layout looks:

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
 
 

Utilities for basic
===========================

Utilities related to building and running--mostly running and gathering numbers from GC and the app itself--numbers of events and so on.

There's a lot of redundancy in these, and I hope to get these cleaned up soon.

Build them using "for f in test*hs ; do ghc --make -O2 ${f} ; done".

buildBunches: build for O2, O2 prof, O2 threaded, O2 threaded eventlog in "./build". Run with -c to configure.

TestN: Run with different values of N.

TestN-strong: Run with different values of N, scaling up the number of particles, P, to keep P/N constant.

test-seed: test the effect of running with a different seed every time. Interesting--suggests we're quire poorly resolved.



This directory contains a moderately capable neutrino Monte Carlo transport code. Features include:
* Core library in src/, driver applications in apps/
* Particles are transported on a spherical, one-dimensional mesh;
* Transport is relativistic: collisions computed in co-moving frame, energy and momentum deposition tallied in the lab frame.
* Analytic cross-sections.
* Counter-based pseudo-random number generator.
* Capable of transporting electron, muon, and tau (anti)neutrinos
* Mesh and fields read from files (see the data/ directory)
* Tools to evaluate performance, garbage collection stats, etc. in utils.

To get started
--------------

1. clone
2. cd McPhD/basic
3. cabal configure -O2
4. cabal build
5. ./dist/build/basic-test/basic-test

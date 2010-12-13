### Makefile --- 

## Author: mwbuksas@lanl.gov
## Version: $Id: Makefile,v 0.0 2010/12/10 22:26:28 mwbuksas Exp $
## Keywords: 
## X-URL: 

.PHONY : main nullstreaming simplestreaming meshtests

all: main nullstreaming meshtests

main: Main.hs
	ghc -o ../bin/$@ -outputdir ../../build/ --make $<

nullstreaming: src/Test/NullStreaming.hs
	ghc -o ../bin/$@ -outputdir ../../build/ --make $<

simplestreaming: src/Test/SimpleStreaming.hs
	ghc -o ../bin/$@ -outputdir ../../build/ --make $<

meshtests: src/Mesh/Tests.hs
	ghc -o ../bin/$@ -outputdir ../../build/ --make $<


### Makefile ends here

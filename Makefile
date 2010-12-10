### Makefile --- 

## Author: mwbuksas@lanl.gov
## Version: $Id: Makefile,v 0.0 2010/12/10 22:26:28 mwbuksas Exp $
## Keywords: 
## X-URL: 

.PHONY : main nullstreaming

all: main nullstreaming

main: Main.hs
	ghc -o ../bin/$@ -outputdir ../build/  --make $<

nullstreaming: NullStreaming.hs
	ghc -o ../bin/$@ -outputdir ../build/  --make $<


### Makefile ends here

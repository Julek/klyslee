all: Main.hs
	ghc --make -XGeneralizedNewtypeDeriving -O -o klys Main.hs
	rm *.o *.hi Klyslee/*.o Klyslee/*.hi

all: Main.hs
	ghc --make -O -o klys Main.hs
	rm *.o *.hi Klyslee/*.o Klyslee/*.hi

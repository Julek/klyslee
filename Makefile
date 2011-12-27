all: Main.hs
	ghc --make -o klys Main.hs
	rm *.o *.hi Klyslee/*.o Klyslee/*.hi

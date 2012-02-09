SRC 	= Main.hs
HC 		= ghc
FLAGS	= --make -XGeneralizedNewtypeDeriving -XFlexibleContexts -O
EXE = klys

all: Main.hs
	$(HC)  $(FLAGS) -o $(EXE) $(SRC)
	rm *.o *.hi Klyslee/*.o Klyslee/*.hi

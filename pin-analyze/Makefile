SOURCES=$(wildcard *.hs)
EXE=pinalyze


DEFAULT: pinalyze

Opcodes.hs: Opcodes.txt mkOpCodes.pl
	./mkOpCodes.pl Opcodes.txt > Opcodes.hs

pinalyze: $(SOURCES)
	ghc --make -o $(EXE) $(SOURCES) 

clean:
	rm -f *.o *.hi $(EXE) *.gnuplot *.dat *.eps

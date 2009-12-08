SOURCES:=$(wildcard *.hs)
EXE:=pinalyze
KMEANS_DIR:=kmeans-0.1.1
KMEANS_SRCS:=$(KMEANS_DIR)/Data/KMeans.hs

DEFAULT: pinalyze

Opcodes.hs: Opcodes.txt mkOpCodes.pl
	./mkOpCodes.pl Opcodes.txt > Opcodes.hs

pinalyze: $(SOURCES)
	ghc -O --make -o $(EXE) $(SOURCES) 

clean:
	rm -f *.o *.hi $(EXE) *.gnuplot *.dat *.eps __PinAlyze__.*
	rm -f $(KMEANS_DIR)/Data/*.o $(KMEANS_DIR)/Data/*.hi

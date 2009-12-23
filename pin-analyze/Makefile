SOURCES:=$(wildcard *.hs)
EXE:=pinalyze
LIBSVMDIR:=libsvm-2.9
LIBSVMSRCS:=$(wildcard $LIBSVMDIR/*.cpp $LIBSVMDIR/*.c)
LIBSVMPROGS:=$(LIBSVMDIR)/svm-train $(LIBSVMDIR)/svm-predict

DEFAULT: pinalyze $(LIBSVMPROGS)

Opcodes.hs: Opcodes.txt mkOpCodes.pl
	./mkOpCodes.pl Opcodes.txt > Opcodes.hs

pinalyze: $(SOURCES)
	ghc -Wall -O --make -o $(EXE) $(SOURCES) 

$(LIBSVMPROGS): $(LIBSVMSRCS)
	make -C $(LIBSVMDIR)

clean:
	rm -f *.o *.hi $(EXE) *.gnuplot *.dat *.eps __PinAlyze__.*
	make -C libsvm-2.9 clean

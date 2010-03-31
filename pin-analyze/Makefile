SOURCES:=$(wildcard *.hs)
GENERATED_SOURCES:=OpcodeGroup.hs Opcodes.hs
EXE:=pinalyze
LIBSVMDIR:=libsvm-2.9
LIBSVMSRCS:=$(wildcard $LIBSVMDIR/*.cpp $LIBSVMDIR/*.c)
LIBSVMPROGS:=$(LIBSVMDIR)/svm-train $(LIBSVMDIR)/svm-predict
MKGRPDIR:=opcode-bucket

DEFAULT: pinalyze $(LIBSVMPROGS)

Opcodes.hs: Opcodes.txt mkOpCodes.pl
	perl mkOpCodes.pl Opcodes.txt > Opcodes.hs

OpcodeGroup.hs: $(MKGRPDIR)/x86reference.xml $(MKGRPDIR)/*.hs
	make -C $(MKGRPDIR) OpcodeGroup > $@

pinalyze: $(SOURCES) $(GENERATED_SOURCES)
	ghc -Wall -O --make -o $(EXE) $(SOURCES) 

$(LIBSVMPROGS): $(LIBSVMSRCS)
	make -C $(LIBSVMDIR)

clean:
	rm -f *.o *.hi $(EXE) *.gnuplot *.dat *.eps __PinAlyze__.*
	make -C $(LIBSVMDIR) clean
	make -C $(MKGRPDIR) clean

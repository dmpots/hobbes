SOURCES:=$(wildcard *.hs)
GENERATED_SOURCES=$(PINALYZE_DIR)/OpcodeGroup.hs $(PINALYZE_DIR)/Opcodes.hs
EXE:=pinalyze
LIBSVMDIR:=libsvm-2.9
LIBSVMSRCS:=$(wildcard $LIBSVMDIR/*.cpp $LIBSVMDIR/*.c)
LIBSVMPROGS:=$(LIBSVMDIR)/svm-train $(LIBSVMDIR)/svm-predict
MKGRPDIR:=opcode-bucket
GHC=ghc
PINALYZE_DIR=pinalyze-src

DEFAULT: pinalyze $(LIBSVMPROGS)

$(PINALYZE_DIR)/Opcodes.hs: $(PINALYZE_DIR)/Opcodes.txt mkOpCodes.pl
	perl mkOpCodes.pl $(PINALYZE_DIR)/Opcodes.txt > $(PINALYZE_DIR)/Opcodes.hs

$(PINALYZE_DIR)/OpcodeGroup.hs: $(MKGRPDIR)/x86reference.xml $(MKGRPDIR)/*.hs
	make -C $(MKGRPDIR) OpcodeGroup > $@

dist/setup-config:
	cabal configure

pinalyze: $(SOURCES) $(GENERATED_SOURCES) dist/setup-config
	cabal build
	cp dist/build/pinalyze/pinalyze .

$(LIBSVMPROGS): $(LIBSVMSRCS)
	make -C $(LIBSVMDIR)

clean:
	rm -f *.o *.hi $(EXE) *.gnuplot *.dat *.eps __PinAlyze__.* pinalyze
	rm -rf dist
	make -C $(LIBSVMDIR) clean
	make -C $(MKGRPDIR) clean

SOURCES:=$(wildcard *.hs)
EXE:=mkBuckets

DEFAULT: $(EXE)

$(EXE): $(SOURCES)
	@ ghc -Wall -O -i.. --make -o $(EXE) $(SOURCES) 

OpcodeGroup: $(EXE)
	@ ./$(EXE) x86reference.xml -
  


clean:
	rm -f *.o *.hi $(EXE) 
	

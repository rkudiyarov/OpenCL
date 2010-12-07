EXECUTABLE	:= test-c2hs
HSMAIN		:= main.hs

C2HS		:= c2hs
GHC		:= ghc

C2HSPATH	:= ../../Foreign/OpenCL/Raw
C2HSFILES	:= $(C2HSPATH)/V10/Error.chs $(C2HSPATH)/V10/Types.chs $(C2HSPATH)/V10/Device.chs $(C2HSPATH)/V10/Platform.chs

GHCFLAGS	:= -XDeriveDataTypeable

LIB		:= -lOpenCL

$(EXECUTABLE) : $(C2HSPATH)/C2HS.hs $(C2HSFILES:.chs=.hs) $(HSMAIN)
	$(GHC) $(GHCFLAGS) -o $@ $^ $(LIB)

%.hs : %.chs
	$(C2HS) $<

clean : 
	rm -f $(C2HSFILES:.chs=*.o)
	rm -f $(C2HSFILES:.chs=*hi)
	rm -f $(EXECUTABLE)
	rm -f $(C2HSFILES:.chs=.hs)
	rm -f $(C2HSFILES:.chs=.chs.*)
	rm -f $(C2HSPATH)/C2HS.hi
	rm -f $(C2HSPATH)/C2HS.o
	rm -f $(HSMAIN:.hs=.o)
	rm -f $(HSMAIN:.hs=.hi)
TARGET		= $(shell uname)

EXECUTABLE	:= oclMatrixMul
HSMAIN		:= $(EXECUTABLE).hs
TESTUTILS	:= ../testUtils.hs

GHC		:= ghc

ifeq ($(TARGET),Darwin)
LIB		:= -framework OpenCL
else
LIB		:= -lOpenCL
endif

$(EXECUTABLE) : $(HSMAIN)
	$(GHC) --make -o $@ $^ $(LIB) $(TESTUTILS)

clean : 
	rm -f $(EXECUTABLE)
	rm -f $(HSMAIN:.hs=.o)
	rm -f $(HSMAIN:.hs=.hi)

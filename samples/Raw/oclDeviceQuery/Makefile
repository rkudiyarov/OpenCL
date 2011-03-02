TARGET		= $(shell uname)

EXECUTABLE	:= oclDeviceQuery
HSMAIN		:= oclDeviceQuery.hs

GHC		:= ghc

ifeq ($(TARGET),Darwin)
LIB		:= -framework OpenCL
else
LIB		:= -lOpenCL
endif

$(EXECUTABLE) : $(HSMAIN)
	$(GHC) --make -o $@ $^ $(LIB)

clean : 
	rm -f $(EXECUTABLE)
	rm -f $(HSMAIN:.hs=.o)
	rm -f $(HSMAIN:.hs=.hi)
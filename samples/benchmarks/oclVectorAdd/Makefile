TARGET		= $(shell uname)

EXECUTABLE	:= oclVectorAdd
HSMAIN		:= oclVectorAdd.hs
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
	rm -f *.o
	rm -f *.hi

c :
	gcc -o $(EXECUTABLE)-c $(LIB) $(EXECUTABLE).c

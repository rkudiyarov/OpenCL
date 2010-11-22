OPENCLPATH	:= /home/kondr/.NVIDIA_GPU_Computing_SDK/OpenCL
EXECUTABLE	:= test-c2hs
HSMAIN		:= main.hs

C2HS		:= c2hs
CC		:= gcc
GHC		:= ghc

C2HSPATH	:= ../../Foreign/OpenCL/Raw
C2HSFILES	:= $(C2HSPATH)/Internal/Types.chs $(C2HSPATH)/V10/Device.chs $(C2HSPATH)/V10/Platform.chs $(C2HSPATH)/Error.chs

INCLUDES	+= -I./ -I$(OPENCLPATH)/common/inc

LIBPATH		:= -L./ -L/usr/lib -L$(OPENCLPATH)/common/lib

$(EXECUTABLE) : $(C2HSFILES:.chs=.hs) $(HSMAIN) $(C2HSPATH)/Internal/C2HS.hs
	$(GHC) -o $@ $^ $(INCLUDES) $(LIBPATH)

%.hs : %.chs
	$(C2HS) $<

clean : 
	rm -f $(C2HSFILES:.chs=*.o)
	rm -f $(C2HSFILES:.chs=*hi)
	rm -f $(EXECUTABLE)
	rm -f $(C2HSFILES:.chs=.hs)
	rm -f $(C2HSFILES:.chs=.chs.*)
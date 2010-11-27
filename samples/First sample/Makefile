EXECUTABLE	:= test-c2hs
HSMAIN		:= main.hs

C2HS		:= c2hs
CC		:= gcc
GHC		:= ghc

C2HSPATH	:= ../../Foreign/OpenCL/Raw
C2HSFILES	:= $(C2HSPATH)/Error.chs $(C2HSPATH)/Internal/Types.chs $(C2HSPATH)/V10/Device.chs $(C2HSPATH)/V10/Platform.chs

GHCFLAGS	:= -XDeriveDataTypeable -XScopedTypeVariables -XEmptyDataDecls -XFlexibleInstances -XTypeSynonymInstances -XOverloadedStrings

INCLUDES	+= -I./ -I/home/kondr/.NVIDIA_GPU_Computing_SDK/OpenCL/common/inc

LIBPATH		:= -L./ -L/usr/lib -L/home/kondr/.NVIDIA_GPU_Computing_SDK/OpenCL/common/lib

LIB		:= -lOpenCL

$(EXECUTABLE) : $(C2HSPATH)/Internal/C2HS.hs $(C2HSFILES:.chs=.hs) $(HSMAIN)
	$(GHC) $(GHCFLAGS) -o $@ $^ $(INCLUDES) $(LIBPATH) $(LIB) /home/kondr/Programs/GHC/lib/ghc-6.12.3/bytestring-0.9.1.7/HSbytestring-0.9.1.7.o

%.hs : %.chs
	$(C2HS) $<

clean : 
	rm -f $(C2HSFILES:.chs=*.o)
	rm -f $(C2HSFILES:.chs=*hi)
	rm -f $(EXECUTABLE)
	rm -f $(C2HSFILES:.chs=.hs)
	rm -f $(C2HSFILES:.chs=.chs.*)
	rm -f $(C2HSPATH)/Internal/C2HS.hi
	rm -f $(C2HSPATH)/Internal/C2HS.o
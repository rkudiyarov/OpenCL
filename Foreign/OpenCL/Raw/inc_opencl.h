#ifndef __INCOPENCL_H
#define __INCOPENCL_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __APPLE__

#undef __BLOCKS__
#include <OpenCL/OpenCL.h>

#else

#include <CL/opencl.h>

#endif

#ifdef __cplusplus
}
#endif

#endif  /* __INCOPENCL_H   */

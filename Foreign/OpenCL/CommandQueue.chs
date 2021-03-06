--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2012] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

#include <inc_opencl.h>

module Foreign.OpenCL.CommandQueue
       ( clCreateCommandQueue
       , clRetainCommandQueue
       , clReleaseCommandQueue
       , clGetCommandQueueContext
       , clGetCommandQueueDevice
       , clGetCommandQueueReferenceCount
       , clGetCommandQueueProperties

#ifndef CL_VERSION_1_1
       , clSetCommandQueueProperty
#endif

#ifndef CL_VERSION_1_2
       , clEnqueueMarker
       , clEnqueueWaitForEvents
       , clEnqueueBarrier
#endif

       , clFlush
       , clFinish
       )
       where

import qualified Foreign.OpenCL.Raw as Raw
import Foreign.OpenCL.Raw.C2HS

import Foreign.OpenCL.Error
import Foreign.OpenCL.Utils

clCreateCommandQueue :: Raw.CL_context -> Raw.CL_device_id -> [Raw.CLCommandQueueProperties] -> IO Raw.CL_command_queue
clCreateCommandQueue c d ps =
    alloca $ \p_err ->
        do
        cq <- Raw.clCreateCommandQueue c d (combineBitMasks ps) p_err
        retCode <- peek p_err
        clCheckError retCode $ return cq

clRetainCommandQueue :: Raw.CL_command_queue -> IO ()
clRetainCommandQueue = clRetain Raw.clRetainCommandQueue

clReleaseCommandQueue :: Raw.CL_command_queue -> IO ()
clReleaseCommandQueue = clRelease Raw.clReleaseCommandQueue

clGetCommandQueueContext :: Raw.CL_command_queue -> IO Raw.CL_context
clGetCommandQueueContext = clGetInfoObject Raw.clGetCommandQueueInfo Raw.CLQueueContext

clGetCommandQueueDevice :: Raw.CL_command_queue -> IO Raw.CL_device_id
clGetCommandQueueDevice = clGetInfoObject Raw.clGetCommandQueueInfo Raw.CLQueueDevice

clGetCommandQueueReferenceCount :: (Integral i) => Raw.CL_command_queue -> IO i
clGetCommandQueueReferenceCount = clGetInfoIntegral Raw.clGetCommandQueueInfo Raw.CLQueueReferenceCount

clGetCommandQueueProperties :: Raw.CL_command_queue -> IO [Raw.CLCommandQueueProperties]
clGetCommandQueueProperties = clGetInfoBitfield Raw.clGetCommandQueueInfo Raw.CLQueueProperties

clFlush :: Raw.CL_command_queue -> IO ()
clFlush = simpleFunction Raw.clFlush

clFinish :: Raw.CL_command_queue -> IO ()
clFinish = simpleFunction Raw.clFinish

-- Deprecated

#ifndef CL_VERSION_1_1

clSetCommandQueueProperty :: Raw.CL_command_queue -> [Raw.CLCommandQueueProperties] -> Raw.CLBool -> IO [Raw.CLCommandQueueProperties]
clSetCommandQueueProperty cq nps b =
    alloca $ \p_ops ->
        do
        retCode <- Raw.clSetCommandQueueProperty cq (combineBitMasks nps) (cFromEnum b) p_ops
        ops <- peek p_ops
        clCheckError retCode $ return $ extractBitMasks ops

#endif

#ifndef CL_VERSION_1_2

clEnqueueMarker :: Raw.CL_command_queue -> IO Raw.CL_event
clEnqueueMarker cq =
    alloca $ \p_e ->
        do
        retCode <- Raw.clEnqueueMarker cq p_e
        clCheckError retCode $ peek p_e

clEnqueueWaitForEvents :: Raw.CL_command_queue -> [Raw.CL_event] -> IO ()
clEnqueueWaitForEvents cq evs =
    do
    let num_events = length evs
    event_list <- newArray evs
    retCode <- Raw.clEnqueueWaitForEvents cq (Raw.cl_uint num_events) event_list
    free event_list
    clCheckError retCode $ return ()

clEnqueueBarrier :: Raw.CL_command_queue -> IO ()
clEnqueueBarrier = simpleFunction Raw.clEnqueueBarrier

#endif

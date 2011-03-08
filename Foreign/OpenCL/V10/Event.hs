--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

module Foreign.OpenCL.V10.Event
       ( ExecutionStatus (..)
       , clWaitForEvents
       , clGetEventCommandQueue
       , clGetEventCommandType
       , clGetEventCommandExecutionStatus
       , clGetEventReferenceCount
       , clRetainEvent
       , clReleaseEvent
       , clGetEventProfilingCommandQueued
       , clGetEventProfilingCommandSubmit
       , clGetEventProfilingCommandStart
       , clGetEventProfilingCommandEnd
       )
       where

import qualified Foreign.OpenCL.Raw.V10 as Raw
import Foreign.OpenCL.Raw.C2HS

import Foreign.OpenCL.V10.Error
import Foreign.OpenCL.V10.Utils

data ExecutionStatus = ExecutionStatusValid Raw.CLExecutionStatus
                     | ExecutionStatusError Raw.CLError
                     deriving (Show, Eq)

clWaitForEvents :: [Raw.CL_event] -> IO ()
clWaitForEvents evs =
    do
    let num_events = length evs
    event_list <- newArray evs
    retCode <- Raw.clWaitForEvents (Raw.cl_uint num_events) event_list
    free event_list
    clCheckError retCode $ return ()

clGetEventCommandQueue :: Raw.CL_event -> IO Raw.CL_command_queue
clGetEventCommandQueue = clGetInfoObject Raw.clGetEventInfo Raw.CLEventCommandQueue

clGetEventCommandType :: Raw.CL_event -> IO Raw.CLCommandType
clGetEventCommandType = clGetInfoEnum Raw.clGetEventInfo Raw.CLEventCommandType

clGetEventCommandExecutionStatus :: Raw.CL_event -> IO ExecutionStatus
clGetEventCommandExecutionStatus e =
    do
    retStatus <- clGetInfoIntegral Raw.clGetEventInfo Raw.CLEventCommandExecutionStatus e
    if retStatus < 0
        then return $ ExecutionStatusError (cToEnum retStatus)
        else return $ ExecutionStatusValid (cToEnum retStatus)

clGetEventReferenceCount :: (Integral i) => Raw.CL_event -> IO i
clGetEventReferenceCount = clGetInfoIntegral Raw.clGetEventInfo Raw.CLEventReferenceCount

clRetainEvent :: Raw.CL_event -> IO ()
clRetainEvent = clRetain Raw.clRetainEvent

clReleaseEvent :: Raw.CL_event -> IO ()
clReleaseEvent = clRelease Raw.clReleaseEvent

clGetEventProfilingInfoIntegral :: (Integral i) => Raw.CLProfilingInfo -> Raw.CL_event -> IO i
clGetEventProfilingInfoIntegral = clGetInfoIntegral Raw.clGetEventProfilingInfo

clGetEventProfilingCommandQueued :: (Integral i) => Raw.CL_event -> IO i
clGetEventProfilingCommandQueued = clGetEventProfilingInfoIntegral Raw.CLProfilingCommandQueued

clGetEventProfilingCommandSubmit :: (Integral i) => Raw.CL_event -> IO i
clGetEventProfilingCommandSubmit = clGetEventProfilingInfoIntegral Raw.CLProfilingCommandSubmit

clGetEventProfilingCommandStart :: (Integral i) => Raw.CL_event -> IO i
clGetEventProfilingCommandStart = clGetEventProfilingInfoIntegral Raw.CLProfilingCommandStart

clGetEventProfilingCommandEnd :: (Integral i) => Raw.CL_event -> IO i
clGetEventProfilingCommandEnd = clGetEventProfilingInfoIntegral Raw.CLProfilingCommandEnd

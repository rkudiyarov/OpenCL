--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2012] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

module Foreign.OpenCL.Error
       ( clCheckError
       )
       where

import qualified Foreign.OpenCL.Raw as Raw
import Foreign.OpenCL.Raw.C2HS

import Control.Exception

instance Exception Raw.CLError

clCheckError :: (Integral i) => i -> IO a -> IO a
clCheckError retCode result = do
    let err = cToEnum retCode
    if err == Raw.CLSuccess
       then result
       else throwIO $ err

--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

module Foreign.OpenCL.HighLevel.V10.Error
       ( clCheckError
       )
       where

import qualified Foreign.OpenCL.Raw.V10 as Raw
import Foreign.OpenCL.Raw.C2HS

import Control.Exception
import Data.Typeable

instance Data.Typeable.Typeable Raw.CLError
instance Exception Raw.CLError

clCheckError :: (Integral i) => i -> IO a -> IO a
clCheckError retCode result = do
    let err = cToEnum retCode
    if err == Raw.CLSuccess
       then result
       else throwIO $ err

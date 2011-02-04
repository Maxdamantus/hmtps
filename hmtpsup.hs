{-# INCLUDE "send.h" #-}
{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C

foreign import ccall "initmtp" c_initmtp :: IO ()
foreign import ccall "endmtp" c_endmtp :: IO ()
foreign import ccall "sendmp3" c_sendmp3 :: CString -> CString -> IO ()

main = do
  putStrLn "foo"
  c_initmtp
  c_endmtp

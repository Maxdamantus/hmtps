{-# LANGUAGE ForeignFunctionInterface #-}

module SendFFI (initMTP, endMTP, sendMP3) where

import Foreign.C

foreign import ccall "initmtp" initMTP :: IO ()
foreign import ccall "endmtp" endMTP :: IO ()
foreign import ccall "sendmp3" c_sendmp3 :: CString -> CString -> IO ()

sendMP3 from to = withCString from $ \cfrom -> withCString to $ \cto -> c_sendmp3 cfrom cto

main = do
  putStrLn "foo"
  initMTP
  sendMP3 "/hd/media/music/Cujo/Adventures In Foam/06 A Vida.mp3" "test.mp3"
  endMTP

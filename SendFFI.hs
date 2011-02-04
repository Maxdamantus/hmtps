{-# LANGUAGE ForeignFunctionInterface #-}

module SendFFI (initMTP, endMTP, sendMP3, listTracks, deleteTrack) where

import Foreign.C
import System.FilePath

foreign import ccall "initmtp" c_initMTP :: IO CInt
foreign import ccall "endmtp" endMTP :: IO ()
foreign import ccall "sendmp3" c_sendmp3 :: CString -> CString -> IO CInt
foreign import ccall "gettracks" c_gettracks :: IO ()
foreign import ccall "hastrack" c_hastrack :: IO CInt
foreign import ccall "curtrack" c_curtrack :: IO CString
foreign import ccall "curtrackn" c_curtrackn :: IO CInt
foreign import ccall "nexttrack" c_nexttrack :: IO ()
foreign import ccall "delete" c_delete :: CInt -> IO CInt

initMTP = c_initMTP >>= return . (== 0)

sendMP3 from to = withCString from $ \cfrom -> withCString to $ \cto -> c_sendmp3 cfrom cto >>= return . (== 0)

loopTracks = do
  has <- c_hastrack
  if has == 1
    then c_curtrack >>= peekCAString >>= \track -> do { num <- c_curtrackn >>= return . fromIntegral; c_nexttrack; rest <- loopTracks; return $ (dropExtension track, num) : rest }
    else return []

deleteTrack id = c_delete (fromIntegral id) >>= return . fromIntegral

listTracks = c_gettracks >> loopTracks

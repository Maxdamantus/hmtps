{-# LANGUAGE ForeignFunctionInterface #-}

module SendFFI (initMTP, endMTP, sendMP3, listTracks, delete, listPlaylists, makePlaylist) where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import System.FilePath
import Control.Arrow
import Control.Monad
import Data.Word

foreign import ccall "initmtp" c_initMTP :: IO CInt
foreign import ccall "endmtp" endMTP :: IO ()
foreign import ccall "sendmp3" c_sendmp3 :: CString -> CString -> IO CInt

foreign import ccall "gettracks" c_gettracks :: IO ()
foreign import ccall "hastrack" c_hastrack :: IO CInt
foreign import ccall "curtrack" c_curtrack :: IO CString
foreign import ccall "curtrackn" c_curtrackn :: IO CInt
foreign import ccall "nexttrack" c_nexttrack :: IO ()

foreign import ccall "getplaylists" c_getplaylists :: IO ()
foreign import ccall "hasplaylist" c_hasplaylist :: IO CInt
foreign import ccall "curplaylist" c_curplaylist :: IO CString
foreign import ccall "curplaylistn" c_curplaylistn :: IO CInt
foreign import ccall "nextplaylist" c_nextplaylist :: IO ()

foreign import ccall "makeplaylist" c_makeplaylist :: CString -> Ptr Word32 -> CInt -> IO ()

foreign import ccall "delete" c_delete :: CInt -> IO CInt

initMTP = c_initMTP >> return () -- >>= return . (== 0)

sendMP3 from to = withCString from $ \cfrom -> withCString to $ \cto -> c_sendmp3 cfrom cto >> return () -- >>= return . (/= 0)

loopItems getNum getStr getHas setNext = do
  has <- getHas
  if has == 1
    then getStr >>= peekCAString >>= \str -> do 
      num <- getNum >>= return . fromIntegral
      setNext
      rest <- loopItems getNum getStr getHas setNext
      return $ (str, num) : rest
    else return []

delete id = c_delete (fromIntegral id) >>= return . fromIntegral

listTracks = c_gettracks >> liftM (map (first dropExtension)) (loopItems c_curtrackn c_curtrack c_hastrack c_nexttrack)
listPlaylists = c_getplaylists >> loopItems c_curplaylistn c_curplaylist c_hasplaylist c_nextplaylist

makePlaylist name ids = withCString name $ \cname -> withArrayLen (map fromIntegral ids) $ \len cids -> c_makeplaylist cname cids (fromIntegral len)

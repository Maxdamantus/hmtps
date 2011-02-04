import Common
import SendFFI

import Control.Monad
import qualified Data.Map as M
import Data.Char
import System.FilePath
import System.Directory
import System.Posix
import System.Cmd

convert src dest = rawSystem "/usr/local/mplayer/bin/ffmpeg" ["-i", src, "-vn", "-sn", "-aq", "0", "-y", dest]

sendTrack fn hashname = do
  let ext = map toLower $ takeExtension fn
  if ext == ".mp3"
    then sendMP3 fn hashname >> return ()
    else do
      tmp <- getProcessID >>= \n -> return $ "/tmp/hmtps-" ++ show n ++ ".mp3"
      convert fn tmp
      sendMP3 tmp hashname
      removeFile tmp


main = getResources >>= \res -> case res of
  Nothing -> return ()
  Just (musicdir, dbfile, tmpdbfile) -> do
    db <- readFile dbfile >>= return . M.fromList . map ((\(a, (b, c)) -> (b, a)) . (read :: String -> (FilePath, (String, Integer)))) . lines
    initMTP
    tracks <- listTracks >>= return . M.fromList
    forM (map snd . M.toList $ M.difference tracks db) deleteTrack
    forM (M.toList $ M.difference db tracks) $ \(hash, fn) -> putStrLn ("Sending " ++ fn) >> sendTrack (musicdir </> fn) (hash <.> "mp3")
    endMTP

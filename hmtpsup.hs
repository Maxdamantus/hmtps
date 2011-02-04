import Common
import SendFFI

import Control.Monad
import qualified Data.Map as M
import System.FilePath

-- (fn, (hash, mt))

main = getResources >>= \res -> case res of
  Nothing -> return ()
  Just (musicdir, dbfile, tmpdbfile) -> do
    db <- readFile dbfile >>= return . M.fromList . map ((\(a, (b, c)) -> (b, a)) . (read :: String -> (FilePath, (String, Integer)))) . lines
    initMTP
    tracks <- listTracks >>= return . M.fromList
    forM (map snd . M.toList $ M.difference tracks db) deleteTrack
    forM (M.toList $ M.difference db tracks) $ \(hash, fn) -> sendMP3 (musicdir </> fn) (hash <.> "mp3")
    endMTP

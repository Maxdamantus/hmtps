import Common
import SendFFI

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import System.FilePath

main = getResources >>= \res -> case res of
  Nothing -> return ()
  Just (musicdir, dbfile, tmpdbfile) -> do
    db <- readFile dbfile >>= return . map ((\(a, (b, c)) -> (filter (/= '/') . head $ splitPath a, b)) . (read :: String -> (FilePath, (String, Integer)))) . lines
    let playlists = M.toList $ foldr (\(pl, hash) -> M.alter (Just . (hash:) . fromMaybe []) pl) M.empty db
    initMTP
    listPlaylists >>= mapM_ (delete . snd)
    tracks <- liftM M.fromList listTracks
    forM_ playlists $ \(pl, hashes) ->
      makePlaylist pl $ mapMaybe (flip M.lookup tracks) hashes
    endMTP

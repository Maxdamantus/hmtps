import Control.Monad
import Control.Arrow

import System
import System.IO
import System.Directory
import System.FilePath
import System.Posix
import System.Environment
import System.Console.GetOpt
import System.Cmd

import SendFFI

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Char as C
import Data.Maybe

convert src dest = rawSystem "/usr/local/mplayer/bin/ffmpeg" ["-i", src, "-vn", "-sn", "-aq", "0", "-y", dest]

listFiles pre dir = do
  list <- liftM (map (dir </>) . filter ((/= '.') . head)) $ getDirectoryContents (pre </> dir)
  rest <- forM list $ \path ->
    doesDirectoryExist (pre </> path) >>= \isdir -> if isdir
      then listFiles pre path
      else liftM ((:[]) . (,) path . name) $ getFileStatus (pre </> path)
  return $ concat rest
  where name stat = show (modificationTime stat) ++ "-" ++ show (fileSize stat)

options =
  [ Option ['m'] ["music"]    (ReqArg sendSongs "DIR") "sync music with device",
    Option ['p'] ["playlist"] (ReqArg sendPlaylists "DIR") "produce playlists, copy to device" ]

main = do
  name <- getProgName
  args <- liftM (getOpt Permute options) getArgs
  case args of
    ([], [], [])  -> putStrLn "Nothing to do"
    (o, [], []) -> initMTP >> sequence_ o >> endMTP
    (_, _, _)  -> putStr $ usageInfo name options

sendPlaylists dir = do
  items <- liftM (map $ first (filter (/= '/') . head . splitPath)) $ listFiles dir ""
  listPlaylists >>= mapM_ (delete . snd)
  let playlists = M.toList $ foldr (\(pl, name) -> M.alter (Just . (name:) . fromMaybe []) pl) M.empty items
  tracks <- liftM M.fromList listTracks
  putStrLn "Sending playlists"
  forM_ playlists $ \(pl, hashes) ->
    makePlaylist pl $ mapMaybe (flip M.lookup tracks) hashes

sendSong name path =
  if map C.toLower (takeExtension path) == ".mp3"
    then sendMP3 path name
    else do
      tmp <- liftM (\n -> "/tmp/hmtps-" ++ show n ++ ".mp3") getProcessID
      convert path tmp
      sendMP3 tmp name
      removeFile tmp

sendSongs dir = do
  items <- liftM (map $ \(a, b) -> (b, a)) $ listFiles dir ""
--  items <- liftM (M.fromList . map (\(a, b) -> (b, a))) listFiles dir ""
  tracks <- listTracks
  let itemsm = M.fromList items
  putStrLn "Deleting extra tracks"
  mapM_ (delete . snd) $ filter (flip M.notMember itemsm . fst) tracks
  let tracksm = M.fromList tracks
  let copy = filter (flip M.notMember tracksm . fst) items
  let amount = length copy
  forM_ (zip [1..] copy) $ \(count, (name, path)) -> do
    putStrLn $ "(" ++ show count ++ "/" ++ show amount ++ ") " ++ path
    sendSong name $ dir </> path

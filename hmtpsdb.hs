import Numeric
import Control.Monad
import qualified Data.ByteString as B
import Crypto.Hash.SHA512
import System
import System.IO
import System.Directory
import System.FilePath
import System.Posix.Files
import qualified Data.Map as M

hashFile fn = B.readFile fn >>= return . join (foldr seq) . foldr showHex "" . concat . map ((\(a, b) -> map fromIntegral [a, b]) . (`divMod` 16)) . B.unpack . hash

findSongs pre dir = do
  list <- getDirectoryContents (pre </> dir) >>= return . map (dir </>) . filter ((/= '.') . head)
  rest <- forM list $ \path -> doesDirectoryExist (pre </> path) >>= \isdir -> if isdir
    then findSongs pre path
    else if takeExtension path `elem` [".mp3", ".flac"] 
      then getFileStatus (pre </> path) >>= return . (:[]) . (,) path . floor . realToFrac . accessTime
      else return []
  return $ concat rest

hashSongs pre old new =
  map (\(fn, mt) -> let newhash = hashFile (pre </> fn) >>= \hash -> seq hash $ return (fn, (hash, mt)) in
    case M.lookup fn old of
      Just (hash, omt) -> if omt == mt
        then return (fn, (hash, mt))
        else newhash
      Nothing -> newhash) new

main = do
  args <- getArgs
  if length args  `notElem` [1, 2]
    then getProgName >>= \n -> putStrLn $ n ++ " musicdir [dbfile]"
    else do
      let musicdir:rest = args
      let dbfile = if rest == [] then musicdir </> ".hmtpsdb" else head rest
      let tmpdbfile = dbfile <.> "tmp"
      print (musicdir, dbfile)
      db <- do
        exists <- doesFileExist dbfile
        if exists
          then readFile dbfile >>= return . M.fromList . map (read :: String -> (FilePath, (String, Integer))) . lines
          else return M.empty
      songs <- findSongs musicdir ""
      hnd <- openFile tmpdbfile WriteMode
      forM (hashSongs musicdir db songs) $ \line -> line >>= hPutStrLn hnd . show
      renameFile tmpdbfile dbfile

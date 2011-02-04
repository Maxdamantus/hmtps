module Common where

import System
import System.IO
import System.Directory
import System.FilePath

getResources = do
  args <- getArgs
  if length args  `notElem` [1, 2]
    then do
      n <- getProgName
      putStrLn $ n ++ " musicdir [dbfile]"
      return Nothing
    else do
      let musicdir:rest = args
      let dbfile = if rest == [] then musicdir </> ".hmtpsdb" else head rest
      let tmpdbfile = dbfile <.> "tmp"
      print (musicdir, dbfile)
      return $ Just (musicdir, dbfile, tmpdbfile)

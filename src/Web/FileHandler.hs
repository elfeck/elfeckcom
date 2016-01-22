{-# LANGUAGE OverloadedStrings #-}

module Web.FileHandler where

import qualified Data.Text as T
import System.Directory
import Control.Monad
import Data.List
import Data.Maybe

import Web.Spock.Safe hiding (head, SessionId)


-- Does not check for correct file extension
saveUploadedFile :: UploadedFile -> String -> String -> Int
                    -> IO (Bool, String)
saveUploadedFile (UploadedFile _ _ upath) filesDir fileName access = do
  let fullPath = filesDir ++ "/upload/access/" ++ show access ++
                 "/" ++ fst (splitName fileName)
  fileExists <- doesFileExist (fullPath ++ snd (splitName fileName))
  if fileExists
    then return (False, "file exts")
    else do createDirectoryIfMissing True fullPath
            copyFile upath (fullPath ++ snd (splitName fileName))
            return (True, "file saved")
  where splitName fn = (reverse $ dropWhile (/= '/') (reverse fn),
                        reverse $ takeWhile (/= '/') (reverse fn))


checkFile :: String -> String -> IO (Maybe FilePath)
checkFile filesDir filePath = do
  doesExist <- doesFileExist (filesDir ++ filePath)
  case doesExist of
    True -> return $ Just (filesDir ++ filePath)
    False -> return Nothing

trashFile :: String -> String -> IO (Bool, String)
trashFile filesDir filePath = do
  let fullPath = filesDir ++ "/upload/access" ++ filePath
  fileExists <- doesFileExist fullPath
  if fileExists
    then do renameFile fullPath (filesDir ++ "/upload/trash/" ++
                                 nameOnly filePath)
            return (True, "file deled")
    else return (False, "intl error")
  where nameOnly p = reverse $ takeWhile (/= '/') (reverse p)

getUploadedFileList :: String -> IO [String]
getUploadedFileList filesDir = do
  fls <- getRecursiveFiles basePath
  modDates <- mapM getModificationTime fls
  let sortedFls = map fst (sortOn snd (zip fls modDates))
  let strippedFls = map (stripPrefix basePath) sortedFls
  return $ reverse $ catMaybes strippedFls
  where basePath = filesDir ++ "/upload/access"


getRecursiveFiles :: FilePath -> IO [String]
getRecursiveFiles path = do
  contents <- getDirectoryContents path
  let properContents = map ((path ++ "/") ++)
                       (filter (`notElem` [".", ".."]) contents)
  dirs <- filterM doesDirectoryExist properContents
  fls <- filterM doesFileExist properContents
  if null dirs
    then return fls
    else do rfls <- mapM getRecursiveFiles dirs
            return $ fls ++ (foldl (++) [] rfls)

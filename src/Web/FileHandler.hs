{-# LANGUAGE OverloadedStrings #-}

module Web.FileHandler where

import qualified Data.Text as T
import System.Directory
import Control.Monad
import Data.List
import Data.Maybe

import Web.Spock.Safe hiding (head, SessionId)


-- Does not check for correct file extension
saveUploadedFile :: UploadedFile -> T.Text -> T.Text -> Int
                    -> IO (Bool, String)
saveUploadedFile (UploadedFile _ _ upath) filesDir fileName access = do
  let fullPath = T.unpack $ T.concat [filesDir
                                     , "/upload/access/"
                                     , T.pack $ show access
                                     , "/"
                                     , fst $ splitName fileName]
  fileExists <- doesFileExist $ (fullPath ++
                                 (T.unpack $ snd $ splitName fileName))
  if fileExists
    then return (False, "file exts")
    else do createDirectoryIfMissing True fullPath
            copyFile upath (fullPath ++ (T.unpack $ snd $ splitName fileName))
            return (True, "file saved")
  where splitName fn = (T.reverse $ T.dropWhile (/= '/') (T.reverse fn),
                        T.reverse $ T.takeWhile (/= '/') (T.reverse fn))


checkFile :: String -> String -> IO (Maybe FilePath)
checkFile filesDir filePath = do
  doesExist <- doesFileExist (filesDir ++ filePath)
  case doesExist of
    True -> return $ Just (filesDir ++ filePath)
    False -> return Nothing

getUploadedFileList :: String -> IO [String]
getUploadedFileList filesDir = do
  fls <- getRecursiveFiles basePath
  let strippedFls = map (stripPrefix basePath) fls
  return $ catMaybes strippedFls
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

{-# LANGUAGE OverloadedStrings #-}

module Web.FileHandler where

import qualified Data.Text as T
import System.Directory
import Control.Monad.IO.Class (liftIO)

import Web.Spock.Safe hiding (head, SessionId)


saveFile :: UploadedFile -> T.Text -> T.Text -> Int -> IO (Bool, String)
saveFile (UploadedFile uname utype upath) filesDir fileName access = do
  let fullPath = T.concat [filesDir
                          , "/upload/access/"
                          , T.pack $ show access
                          , "/"]
  fileExists <- doesFileExist $ (T.unpack $ T.append fullPath fileName)
  if fileExists
    then return (False, "File already exists")
    else return (True, "File does not exists")

{-# LANGUAGE OverloadedStrings #-}

module CreateUser where

import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import qualified Data.Text as T

import Model.Types

createUser :: String -> String -> String -> Int -> IO ()
createUser dbpath name pw access = do
  runSqlite (T.pack dbpath) $ do
    runMigration migrateCore
    userId <- insert $ User (T.pack name) (T.pack pw) access
    liftIO $ print ("Inserted User with key: " ++ (show $ fromSqlKey userId))

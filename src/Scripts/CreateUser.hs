{-# LANGUAGE OverloadedStrings #-}

module CreateUser where

import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import qualified Data.Text as T
import System.Random

import Model.Types
import Model.CryptoUtils

createUser :: String -> String -> String -> Int -> IO ()
createUser dbpath name pw access = do
  runSqlite (T.pack dbpath) $ do
    runMigration migrateCore
    g <- liftIO $ getStdGen
    let salt = randomBS 512 g
        hash = hashText (T.pack pw) salt
    userId <- insert $ User (T.pack name) (makeHex hash) (makeHex salt) access
    liftIO $ print ("Inserted User with key: " ++ (show $ fromSqlKey userId))

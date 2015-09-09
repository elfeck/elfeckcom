{-# LANGUAGE OverloadedStrings #-}

module ManualDB where

import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import qualified Data.Text as T
import System.Random
import Data.Maybe

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

addPostToCategory :: String -> IO ()
addPostToCategory dbpath = do
  runSqlite (T.pack dbpath) $ do
    runMigration migrateCore
    rows <- selectList [] [Desc PostModDate]
    let posts = map (\r -> (entityKey r, entityVal r)) rows
    let postsWithCat = filter (\(id, p) -> isJust $ postCategories p) posts
    let pwcFlat = flatten postsWithCat []
    liftIO $ print (map (\(k, v) -> (fromSqlKey k, v)) pwcFlat)
    ptcIds <- mapM (\p -> insert $ PostToCategory (fst p) (snd p)) pwcFlat
    liftIO $ print "Inserted cats with the following keys"
    liftIO $ print (map fromSqlKey ptcIds)

getAllOfCat :: String -> String -> IO ()
getAllOfCat dbpath cat = do
  runSqlite (T.pack dbpath) $ do
    runMigration migrateCore
    ptcRows <- selectList [PostToCategoryCategory ==. (T.pack cat)] []
    let ptcIds = map (\p -> postToCategoryPost $ entityVal p) ptcRows
    postRows <- mapM get ptcIds
    liftIO $ print postRows

flatten :: [(PostId, Post)] -> [(PostId, T.Text)] -> [(PostId, T.Text)]
flatten [] acc = acc
flatten (x : xs) acc =
  flatten xs ([(fst x, cat) | cat <- fromJust (postCategories $ snd x)] ++ acc)

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}

module Model where

import Data.Aeson.Types
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Database.Persist.TH
import qualified Data.Text as T
import Data.Text.Read
import Data.Time
import Data.Maybe
import System.Locale
import Web.Spock.Shared hiding (SessionId)

import Database.Persist.Sql


share [mkPersist sqlSettings, mkMigrate "migrateCore"][persistLowerCase|
Session
  validUntil UTCTime
  userId UserId
  deriving Show
User
  name T.Text
  pass T.Text
  access Int
  UniqueUsername name
  deriving Show
Post json
  title T.Text Maybe
  categories [T.Text] Maybe
  content T.Text
  modDate UTCTime
  crtDate UTCTime
  ptype Int
  access Int
  deriving Show
|]

loginUser :: T.Text -> T.Text -> SqlPersistM (Maybe UserId)
loginUser name pass = do
  mName <- getBy (UniqueUsername name)
  case mName of
   Just row ->
     if (userPass (entityVal row)) == pass
     then return $ Just (entityKey row)
     else return Nothing
   Nothing ->
     return Nothing

logoutUser :: UserId -> SqlPersistM ()
logoutUser userId = deleteWhere [SessionUserId ==. userId]

loadUser :: SessionId -> SqlPersistM (Maybe (UserId, User))
loadUser sessId = do
  mSess <- get sessId
  now <- liftIO getCurrentTime
  case mSess of
   Just sess ->
     if sessionValidUntil sess > now
     then do mUser <- get (sessionUserId sess)
             return $ fmap (\user -> (sessionUserId sess, user)) mUser
     else return Nothing
   Nothing -> return Nothing

createSession :: UserId -> SqlPersistM SessionId
createSession userId = do
  now <- liftIO getCurrentTime
  insert (Session (addUTCTime (5 * 3600) now) userId)

createUser :: T.Text -> T.Text -> Int -> SqlPersistM T.Text
createUser name pass access = do
  mName <- getBy (UniqueUsername name)
  case mName of
   Just _ -> return "Username taken"
   Nothing -> do
     insert (User name pass access)
     return "User created"

createPost :: [T.Text] -> SqlPersistM T.Text
createPost postParam = do
  let ty = (procInt $ postParam !! 3)
  let ac = (procInt $ postParam !! 4)
  now <- liftIO $ getCurrentTime
  if isNothing ty || isNothing ac
    then return "Invalid postsubmit"
    else do let post = Post (procTitle $ postParam !! 0)
                       (procCategories $ postParam !! 1)
                       (postParam !! 2)
                       now
                       now
                       (fromJust ty)
                       (fromJust ac)
            insert post
            return "Post successful created"
  where procTitle "" = Nothing
        procTitle text = Just text
        procCategories "" = Nothing
        procCategories text = Just $ T.splitOn ", " text

procInt text = case decimal text of
                Left _ -> Nothing
                Right (val, "") -> Just val
                Right (val, _) -> Nothing

queryAllPosts :: SqlPersistM ([(PostId, Post)])
queryAllPosts = do
  rows <- selectList [] [Desc PostModDate]
  return $ map (\r -> (entityKey r, entityVal r)) rows

queryPost :: [T.Text] -> SqlPersistM (Maybe (PostId, Post))
queryPost (textpid : _) = case procInt textpid of
  Nothing -> return Nothing
  Just pid -> do
    mpost <- get $ toSqlKey pid
    case mpost of
     Nothing -> return Nothing
     Just post -> return $ Just (toSqlKey pid, post)

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) =>
          SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSQL action =
  runQuery $ \conn -> runResourceT $ runNoLoggingT $ runSqlConn action conn

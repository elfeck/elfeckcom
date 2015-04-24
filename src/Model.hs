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
import Data.Time
import Data.Maybe
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
  crtDate UTCTime
  modDate UTCTime
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

queryUser :: SessionId -> SqlPersistM (Maybe (UserId, User))
queryUser sessId = do
  mSess <- get sessId
  now <- liftIO getCurrentTime
  case mSess of
   Just sess ->
     if sessionValidUntil sess > now
     then do mUser <- get (sessionUserId sess)
             return $ fmap (\user -> (sessionUserId sess, user)) mUser
     else return Nothing
   Nothing -> return Nothing

insertSession :: UserId -> SqlPersistM SessionId
insertSession userId = do
  now <- liftIO getCurrentTime
  insert (Session (addUTCTime (5 * 3600) now) userId)

insertUser :: T.Text -> T.Text -> Int -> SqlPersistM T.Text
insertUser name pass access = do
  mName <- getBy (UniqueUsername name)
  case mName of
   Just _ -> return "ney: username taken"
   Nothing -> do
     insert (User name pass access)
     return "yey: user created"

insertPost :: Post -> SqlPersistM T.Text
insertPost post = do
  now <- liftIO $ getCurrentTime
  insert $ adjustTime post now now
  return "yey: created"

deletePost :: Int -> SqlPersistM T.Text
deletePost pid = do
  mpost <- get $ ((toSqlKey $ fromIntegral pid) :: PostId)
  case mpost of
   Nothing -> return "ney: unkwn pid"
   Just _ -> do delete ((toSqlKey $ fromIntegral pid) :: PostId)
                return "yey: deleted"

updatePost :: Int -> Post -> SqlPersistM T.Text
updatePost pid post = do
  mpost <- get $ ((toSqlKey $ fromIntegral pid) :: PostId)
  case mpost of
   Nothing -> return "ney: unkwn pid"
   Just oldpost -> do
     let crtTime = postCrtDate oldpost
     now <- liftIO $ getCurrentTime
     replace ((toSqlKey $ fromIntegral pid) :: PostId) $
       adjustTime post crtTime now
     return "yey: updated"

adjustTime :: Post -> UTCTime  -> UTCTime -> Post
adjustTime (Post t ca co cr md ty ac) cr' md' =
  (Post t ca co cr' md' ty ac)

queryAllPosts :: SqlPersistM ([(PostId, Post)])
queryAllPosts = do
  rows <- selectList [] [Desc PostModDate]
  return $ map (\r -> (entityKey r, entityVal r)) rows

queryPost :: Int -> SqlPersistM (Maybe (PostId, Post))
queryPost pid = do
  mpost <- get $ toSqlKey $ fromIntegral pid
  case mpost of
   Nothing -> return Nothing
   Just post -> return $ Just (toSqlKey $ fromIntegral pid, post)

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) =>
          SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSQL action =
  runQuery $ \conn -> runResourceT $ runNoLoggingT $ runSqlConn action conn

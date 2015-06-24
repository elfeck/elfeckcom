{-# LANGUAGE OverloadedStrings #-}

module Model.Model where

import Data.Aeson.Types
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Time
import Data.Maybe
import Web.Spock.Shared hiding (SessionId)
import Database.Persist.Sql

import Model.Types

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
  insert $ adjustTimePost post now now
  return "yey: created"

insertSystemVisit :: SystemVisit -> SqlPersistM T.Text
insertSystemVisit sysVisit = do
  now <- liftIO $ getCurrentTime
  insert $ adjustTimeSystemVisit sysVisit now
  return "yey: created"

deletePost :: Int -> SqlPersistM T.Text
deletePost pid = do
  mpost <- get $ ((toSqlKey $ fromIntegral pid) :: PostId)
  case mpost of
   Nothing -> return "ney: unkwn pid"
   Just _ -> do delete ((toSqlKey $ fromIntegral pid) :: PostId)
                return "yey: deleted"

deleteSystemVisit :: Int -> SqlPersistM T.Text
deleteSystemVisit eid = do
  mvisit <- get $ ((toSqlKey $ fromIntegral eid) :: SystemVisitId)
  case mvisit of
   Nothing -> return "ney: unkwn eid"
   Just _ -> do delete ((toSqlKey $ fromIntegral eid) :: SystemVisitId)
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
       adjustTimePost post crtTime now
     return "yey: updated"

updateSystemVisit :: Int -> SystemVisit -> SqlPersistM T.Text
updateSystemVisit eid visit = do
  mvisit <- get $ ((toSqlKey $ fromIntegral eid) :: SystemVisitId)
  case mvisit of
   Nothing -> return "ney: unkwn eid"
   Just oldvisit -> do
     replace ((toSqlKey $ fromIntegral eid) :: SystemVisitId) $
       adjustTimeSystemVisit visit (systemVisitCrtDate oldvisit)
     return "yey: updated"

adjustTimePost :: Post -> UTCTime -> UTCTime -> Post
adjustTimePost (Post t ca co cr md ty ac) cr' md' =
  (Post t ca co cr' md' ty ac)

adjustTimeSystemVisit :: SystemVisit -> UTCTime -> SystemVisit
adjustTimeSystemVisit (SystemVisit n r s t a) t' =
  (SystemVisit n r s t' a)

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

queryAllPosts :: SqlPersistM [(PostId, Post)]
queryAllPosts = do
  rows <- selectList [] [Desc PostModDate]
  return $ map (\r -> (entityKey r, entityVal r)) rows

queryPost :: Int -> SqlPersistM (Maybe (PostId, Post))
queryPost pid = do
  mpost <- get $ toSqlKey $ fromIntegral pid
  case mpost of
   Nothing -> return Nothing
   Just post -> return $ Just (toSqlKey $ fromIntegral pid, post)

queryAllVisits :: SqlPersistM [(SystemVisitId, SystemVisit)]
queryAllVisits = do
  rows <- selectList [] [Desc SystemVisitCrtDate]
  return $ map (\r -> (entityKey r, entityVal r)) rows

queryVisit :: Int -> SqlPersistM (Maybe (SystemVisitId, SystemVisit))
queryVisit eid = do
  mvisit <- get $ toSqlKey $ fromIntegral eid
  case mvisit of
   Nothing -> return Nothing
   Just visit -> return $ Just (toSqlKey $ fromIntegral eid, visit)

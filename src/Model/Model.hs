{-# LANGUAGE OverloadedStrings #-}

module Model.Model where

import Control.Monad.IO.Class
import Data.Maybe
import Data.List (nub)
import qualified Data.Text as T
import Data.Time
import Database.Persist.Sql

import Model.Types
import Model.CryptoUtils

loginUser :: T.Text -> T.Text -> SqlPersistM (Maybe UserId)
loginUser name pass = do
  mName <- getBy (UniqueUsername name)
  case mName of
   Just row ->
     let user = entityVal row
     in if (userPass user) ==
           (makeHex $ hashText pass (decodeHex $ userSalt user))
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

insertUser :: User -> SqlPersistM T.Text
insertUser (User name pass salt access) = do
  mName <- getBy (UniqueUsername name)
  case mName of
   Just _ -> return "ney: username taken"
   Nothing -> do
     insert (User name pass salt access)
     return "yey: user created"

insertPost :: Post -> SqlPersistM T.Text
insertPost post = do
  now <- liftIO $ getCurrentTime
  pid <- insert $ adjustTimePost post now now
  if isJust (postCategories post)
    then do let ptc = [PostToCategory pid c |
                       c <- fromJust $ postCategories post]
            mapM_ insert ptc
    else return ()
  return "yey: created"

insertSystemVisit :: SystemVisit -> SqlPersistM T.Text
insertSystemVisit sysVisit = do
  now <- liftIO $ getCurrentTime
  insert $ adjustTimeSystemVisit sysVisit now
  return "yey: created"

deletePost :: Int -> SqlPersistM T.Text
deletePost pid = do
  mpost <- get $ sqlpid
  case mpost of
   Nothing -> return "ney: unkwn pid"
   Just _ -> do delete sqlpid
                ptc <- selectKeysList [PostToCategoryPost ==. sqlpid] []
                mapM_ delete ptc
                return "yey: deleted"
  where sqlpid = (toSqlKey $ fromIntegral pid) :: PostId

deleteSystemVisit :: Int -> SqlPersistM T.Text
deleteSystemVisit eid = do
  mvisit <- get $ ((toSqlKey $ fromIntegral eid) :: SystemVisitId)
  case mvisit of
   Nothing -> return "ney: unkwn eid"
   Just _ -> do delete ((toSqlKey $ fromIntegral eid) :: SystemVisitId)
                return "yey: deleted"

updatePost :: Int -> Post -> SqlPersistM T.Text
updatePost pid post = do
  mpost <- get sqlpid
  case mpost of
   Nothing -> return "ney: unkwn pid"
   Just oldpost -> do
     let crtTime = postCrtDate oldpost
     now <- liftIO $ getCurrentTime
     replace sqlpid $ adjustTimePost post crtTime now
     ptc <- selectKeysList [PostToCategoryPost ==. sqlpid] []
     mapM_ delete ptc
     if isJust $ postCategories post
       then do let ptc = [PostToCategory sqlpid c |
                          c <- fromJust $ postCategories post]
               mapM_ insert ptc
       else return ()
     return "yey: updated"
  where sqlpid = (toSqlKey $ fromIntegral pid) :: PostId

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

queryAllDrivelCategories :: Int -> SqlPersistM [T.Text]
queryAllDrivelCategories access = do
  rows <- selectList [PostAccess <=. access, PostPtype >. 0] []
  let mcats = map (\r -> postCategories (entityVal r)) rows
  let cats = map fromJust $ filter isJust mcats
  return $ nub $ foldl (++) [] cats

queryDrivel :: Int -> (Int, Int) -> [T.Text] -> Bool ->
               SqlPersistM [(PostId, Post)]
-- no categories at all
queryDrivel access (f, t) [] ponly = do
  let ptype = if ponly then 1 else 2
  rows <- selectList [PostAccess <=. access, PostPtype >. 0,
                      PostPtype <=. ptype]
          [Desc PostCrtDate, OffsetBy f, LimitTo (t - f + 1)]
  return $ map (\r -> (entityKey r, entityVal r)) rows

queryDrivel access (f, t) cats ponly = do
  crows <- mapM (\cat -> selectList [PostToCategoryCategory ==. cat] []) cats
  let ptype = if ponly then 1 else 2
  let postIds = map (map (\r -> postToCategoryPost (entityVal r))) crows
  let filtIds = filtCat postIds (nub $ foldl (++) [] postIds)
  prows <- selectList [PostAccess <=. access, PostPtype >. 0,
                       PostPtype <=. ptype, PostId <-. filtIds]
           [Desc PostCrtDate, OffsetBy f, LimitTo (t - f + 1)]
  return $ map (\r -> (entityKey r, entityVal r)) prows

filtCat :: [[PostId]] -> [PostId] -> [PostId]
filtCat postIds candi = [c | c <- candi, and (map (elem c) postIds)]

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

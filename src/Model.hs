{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Database.Persist.TH
import qualified Data.Text as T
import Data.Time
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
Post
  title T.Text Maybe
  categories [T.Text] Maybe
  content T.Text
  modDate UTCTime
  crtDate UTCTime
  ptype Int
  access Int
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
     _ <- insert (User name pass access)
     return "User created"

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) =>
          SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSQL action =
  runQuery $ \conn -> runResourceT $ runNoLoggingT $ runSqlConn action conn

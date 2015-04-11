{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Maybe
import Data.Text.Lazy (toStrict, fromStrict)
import qualified Data.Text as T
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Web.Spock.Safe hiding (head, SessionId)
import Web.Spock.Shared hiding (SessionId)
import Database.Persist.Sqlite hiding (get)

import View
import EntryParser
import Model

data BlogState = BlogState
type SessionVal = Maybe SessionId
type BlogApp = SpockM SqlBackend SessionVal BlogState ()
type BlogAction a = SpockAction SqlBackend SessionVal BlogState a

main :: IO ()
main = do
  svg <- readFile "static/img/header.svg"
  let files = [svg]
  pool <- runNoLoggingT $ createSqlitePool "elfeck.db" 5
  runNoLoggingT $ runSqlPool (runMigration migrateCore) pool
  runSpock 3000 $ spock sessConfig (PCPool pool) BlogState (app files)
    where sessConfig = SessionCfg { sc_cookieName = "elfeckcom"
                                  , sc_sessionTTL = 60 * 5 * 50
                                  , sc_sessionIdEntropy = 40
                                  , sc_emptySession = Nothing
                                  , sc_persistCfg = Nothing
                                  }

app :: [String] -> BlogApp
app files = do
  middleware (staticPolicy (addBase "static"))
  handleGets files
  handlePosts

handleGets ::[String] -> BlogApp
handleGets files = do
  get root $ blaze $ do
    siteHead
    siteHeader (head files)
    testBody
  get "elfeck" $ redirect "/"
  get "edit" $ requireUserRight 5 $ blaze $ do
    siteHead
    infBackHeader (head files) "edit"
    siteEdit
  get "login" $ blaze $ do
    siteHead
    infBackHeader (head files) "login"
    siteLogin
  get "logout" $ do
    user <- loadUserSession
    case user of
     Nothing -> redirect "/"
     Just (userId, _) -> do
       runSQL $ logoutUser userId
       writeSession Nothing
       redirect "/"
  hookAny GET $ \_ -> blaze $ do
    siteHead
    emptyHeader (head files)
    site404

handlePosts :: BlogApp
handlePosts = do
  post "edit/preview" $ do
    dat <- params
    let chkp = checkJson $ findParams dat ["title", "categories", "content"]
    case chkp of
     Nothing -> errorJson
     Just par -> editResponse par
  post "edit/submit" $ do
    return ()
  post "login/submit" $ do
    dat <- params
    let chkp = checkJson $ findParams dat ["name", "pass"]
    case chkp of
     Nothing -> errorJson
     Just par -> do
       login <- runSQL $ loginUser (par !! 0) (par !! 1)
       case login of
        Nothing -> loginResponse False
        Just userId -> do
          sessId <- runSQL $ createSession userId
          writeSession (Just sessId)
          loginResponse True
  where errorJson = json $ ("error in sent json" :: T.Text)

editResponse xs = json $ parseEdit (map fromStrict xs)

loginResponse True =  json (("login success. yey" :: T.Text), True)
loginResponse False =  json (("wrong login data, try again" :: T.Text), False)

findParam :: [(T.Text, T.Text)] -> T.Text -> Maybe T.Text
findParam [] _ = Nothing
findParam ((n, c) : xs) name | n == name' = Just c
                             | otherwise = findParam xs name
  where name' = T.append (T.append "dat[" name) "]"

findParams :: [(T.Text, T.Text)] -> [T.Text] -> [Maybe T.Text]
findParams xs names = map (findParam xs) names

checkJson :: [Maybe T.Text] -> Maybe [T.Text]
checkJson xs | null $ filter isNothing xs = Just (map fromJust xs)
             | otherwise = Nothing

requireLogin :: BlogAction a -> BlogAction a
requireLogin action = do
  user <- loadUserSession
  case user of
   Nothing -> redirect "/login"
   Just _ -> action

requireUserRight :: Int -> BlogAction a -> BlogAction a
requireUserRight reqAccess action = do
  user <- loadUserSession
  case user of
   Nothing -> redirect "/login"
   Just user ->
     case checkUserRight user reqAccess of
      False -> redirect "/accessDenied"
      True -> action

loadUserSession :: BlogAction (Maybe (UserId, User))
loadUserSession = do
  sess <- readSession
  case sess of
   Nothing -> return Nothing
   Just sid -> do mUser <- runSQL $ loadUser sid
                  return mUser

checkUserRight :: (UserId, User) -> Int -> Bool
checkUserRight (_, user) reqAccess = userAccess user >= reqAccess

blaze :: MonadIO m => Html -> ActionT m a
blaze = html . toStrict . renderHtml

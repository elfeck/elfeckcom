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
    where sessConfig =
            SessionCfg { sc_cookieName = "elfeckcom"
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

handleGets :: [String] -> BlogApp
handleGets files = do
  get root $ do
    muser <- loadUserSession
    blaze $ do
      siteHead
      siteHeader (head files)
      testBody
      siteFooter $ fmap snd muser
  get "elfeck" $ redirect "/"
  get "edit" $ do
    muser <- loadUserSession
    posts <- runSQL $ queryAllPosts
    --reqRight muser 5 $
    blaze $ do
      siteHead
      infBackHeader (head files) "edit"
      siteEdit posts
  get "login" $ blaze $ do
    siteHead
    infBackHeader (head files) "login"
    siteLogin
  get "logout" $ do
    muser <- loadUserSession
    case muser of
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
    muser <- loadUserSession
    --reqRight' muser 5 $
    do
      dat <- params
      let chkp = checkJson $ findParams dat ["title", "categories", "content"]
      case chkp of
       Nothing -> errorJson
       Just par -> editResponse par
  post "edit/submit" $ do
    muser <- loadUserSession
    --reqRight' muser 5 $
    do
      dat <- params
      let chkp = checkJson $ findParams dat ["submitType", "pid", "title",
                                             "categories", "content",
                                             "type", "access"]
      case chkp of
       Nothing -> errorJson
       Just par -> submitEdit par
  post "edit/loadpost" $ do
    muser <- loadUserSession
    --reqRight' muser 5 $
    do
      dat <- params
      let chkp = checkJson $ findParams dat ["id"]
      case chkp of
       Nothing -> errorJson
       Just par -> do
         resp <- runSQL $ queryPost par
         loadpostResponse resp
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
          sessId <- runSQL $ insertSession userId
          writeSession (Just sessId)
          loginResponse True
  where errorJson = json $ ("error in sent json" :: T.Text)

submitEdit xs = do
  r <- case (head xs) of
        "0" -> do
          resp <- runSQL $ insertPost (drop 2 xs)
          return resp
        "1" -> do
          resp <- runSQL $ updatePost (tail xs)
          return resp
        "2" -> do
          resp <- runSQL $ deletePost (tail xs)
          return resp
        _ -> return ("ney: unkwn stype")
  submitResponse r

editResponse xs = json $ parseEdit (map fromStrict xs)

loginResponse True =  json (("login success. yey" :: T.Text), True)
loginResponse False =  json (("wrong login data, try again" :: T.Text), False)

submitResponse resp = json resp

loadpostResponse Nothing = json ("could not find post to id" :: T.Text)
loadpostResponse (Just post) = json $ post

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

reqLogin :: Maybe (UserId, User) -> BlogAction a -> BlogAction a
reqLogin Nothing _ = redirect "/login"
reqLogin _ action = action

reqRight :: Maybe (UserId, User) -> Int -> BlogAction a -> BlogAction a
reqRight Nothing _ _ = redirect "/login"
reqRight (Just (_, user)) reqAccess action =
  if checkUserRight user reqAccess
  then action
  else redirect "/accessDenied"

reqRight' :: Maybe (UserId, User) -> Int -> BlogAction a -> BlogAction a
reqRight' Nothing _ _ = json ("post error: user not logged in" :: T.Text)
reqRight' (Just (_, user)) reqAccess action =
  if checkUserRight user reqAccess
  then action
  else json ("post error: user access denied" :: T.Text)

loadUserSession :: BlogAction (Maybe (UserId, User))
loadUserSession = do
  sess <- readSession
  case sess of
   Nothing -> return Nothing
   Just sid -> do mUser <- runSQL $ queryUser sid
                  return mUser

checkUserRight :: User -> Int -> Bool
checkUserRight user reqAccess = userAccess user >= reqAccess

blaze :: MonadIO m => Html -> ActionT m a
blaze = html . toStrict . renderHtml

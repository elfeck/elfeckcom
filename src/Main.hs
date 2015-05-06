{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE GADTs             #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Maybe
import Data.Time
import Data.Monoid
import Data.Text.Lazy (toStrict, fromStrict)
import qualified Data.Text as T
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Web.Spock.Safe hiding (head, SessionId)
import Web.Spock.Shared hiding (SessionId)
import Database.Persist.Sqlite hiding (get)

import View
import PostParser
import Model
import Utils

data BlogState = BlogState
type SessionVal = Maybe SessionId
type BlogApp = SpockM SqlBackend SessionVal SiteConfig ()
type BlogAction a = SpockAction SqlBackend SessionVal SiteConfig a

main :: IO ()
main = do
  svg <- readFile "static/img/header.svg"
  configFile <- readFile "config.txt"
  let config = parseConfig $ T.pack configFile
  let files = [svg]
  pool <- runNoLoggingT $ createSqlitePool (database config) 5
  runNoLoggingT $ runSqlPool (runMigration migrateCore) pool
  runSpock 3000 $ spock sessConfig (PCPool pool) config (app files config)
    where sessConfig =
            SessionCfg { sc_cookieName = "elfeckcom"
                       , sc_sessionTTL = 60 * 5 * 50
                       , sc_sessionExpandTTL = True
                       , sc_sessionIdEntropy = 40
                       , sc_emptySession = Nothing
                       , sc_persistCfg = Nothing
                       }

app :: [String] -> SiteConfig -> BlogApp
app files (SiteConfig _ routes) = do
  middleware (staticPolicy (addBase "static"))
  handleGets files routes
  handlePosts

handleGets :: [String] -> [Route] -> BlogApp
handleGets files staticRoutes = do
  sequence_ $ map (handleStatic files) staticRoutes
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
  get "evexpl" $ do
    muser <- loadUserSession
    visits <- runSQL $ queryAllVisits
    --reqRight muser 5 $
    blaze $ do
      siteHead
      infBackHeader (head files) "evexpl"
      siteEvexpl visits
  hookAny GET $ \_ -> blaze $ do
    siteHead
    emptyHeader (head files)
    site404

handleStatic :: [String] -> Route -> BlogApp
handleStatic _ (Redirect from to) = get (static $ T.unpack from) $ redirect to
handleStatic fs (DB from pid) = get (static $ T.unpack from) $ do
  muser <- loadUserSession
  mpost <- runSQL $ queryPost pid
  case mpost of
   Nothing -> handleInvPid fs
   Just post -> blaze $ do
     siteHead
     siteHeader (head fs)
     siteBody $ parsePost $ snd post
     siteFooter $ fmap snd muser

handleInvPid fs = blaze $ do
  siteHead
  siteHeader (head fs)
  siteInvPid

handlePosts :: BlogApp
handlePosts = do
  post "edit/preview" $ do
    muser <- loadUserSession
    --reqRight' muser 5 $
    do
      dat <- params
      let mpost = jsonToPost dat
      case (mpost) of
       Just post -> previewResponse post
       Nothing -> errorJson
  post "edit/submit" $ do
    muser <- loadUserSession
    --reqRight' muser 5 $
    do
      dat <- params
      let msubmitType = findParam dat "submitType"
      let mpid = fmap textToInt $ findParam dat "pid"
      let mpost = jsonToPost dat
      case (msubmitType, mpid, mpost) of
       (Just st, Just (Just pid), Just post) -> submitEdit st pid post
       _ -> errorJson
  post "edit/loadpost" $ do
    muser <- loadUserSession
    --reqRight' muser 5 $
    do
      dat <- params
      let mpid = fmap textToInt $ findParam dat "pid"
      case mpid of
       Nothing -> errorJson
       Just (Just pid) -> do
         resp <- runSQL $ queryPost pid
         loadpostResponse resp
  post "evexpl/submit" $ do
    muser <- loadUserSession
    --reqRight' muser 5 $
    do
      dat <- params
      let msubmitType = findParam dat "submitType"
      let meid = fmap textToInt $ findParam dat "eid"
      let mvisit = jsonToSystemVisit (fmap snd muser) dat
      case (msubmitType, meid, mvisit) of
       (Just st, Just (Just (eid)), Just visit) -> submitEvexpl st eid visit
       _ -> json $ (show mvisit)
  post "/evexpl/loadvisit" $ do
    muser <- loadUserSession
    --reqRight' muser 5 $
    do
      dat <- params
      let meid = fmap textToInt $ findParam dat "eid"
      case meid of
       Nothing -> errorJson
       Just (Just eid) -> do
         resp <- runSQL $ queryVisit eid
         loadvisitResponse resp
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
  where errorJson = json $ ("ney: json invld" :: T.Text)

submitEdit submitType pid post = do
  r <- case submitType of
        "0" -> do
          resp <- runSQL $ insertPost post
          return resp
        "1" -> do
          resp <- runSQL $ updatePost pid post
          return resp
        "2" -> do
          resp <- runSQL $ deletePost pid
          return resp
        _ -> return "ney: unkwn stype"
  submitResponse r

submitEvexpl submitType eid visit = do
  r <- case submitType of
    "0" -> do
      resp <- runSQL $ insertSystemVisit visit
      return resp
    "1" -> do
      resp <- runSQL $ updateSystemVisit eid visit
      return resp
    "2" -> do
      resp <- runSQL $ deleteSystemVisit eid
      return resp
    _ -> return "ney: unkwn stype"
  submitResponse r

previewResponse post = json $ renderPost post

loginResponse True =  json (("login success. yey" :: T.Text), True)
loginResponse False =  json (("wrong login data, try again" :: T.Text), False)

submitResponse resp = json resp

loadpostResponse Nothing = json ("could not find post to id" :: T.Text)
loadpostResponse (Just post) = json $ post

loadvisitResponse Nothing = json ("could not find visit to id" :: T.Text)
loadvisitResponse (Just visit) = json $ visit

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

jsonToPost :: [(T.Text, T.Text)] -> Maybe Post
jsonToPost params = case chkp of
  Nothing -> Nothing
  Just pars -> createPost pars dummyTime dummyTime
  where chkp = checkJson $ findParams params ["title", "categories",
                                              "content", "type", "access"]

-- Handles possible user error here. Should be somewhere else
-- DummyAuthor for not having to login while testing
jsonToSystemVisit :: Maybe User -> [(T.Text, T.Text)] -> Maybe SystemVisit
jsonToSystemVisit mauthor params = case (mauthor, chkp) of
  (Just author, Just pars) ->
    createSystemVisit (userName author) pars dummyTime
  (Nothing, Just pars) -> createSystemVisit (dummyAuthor) pars dummyTime
  _ -> Nothing
  where chkp = checkJson $ findParams params ["name", "region", "sites",
                                              "types"]
        dummyAuthor = "seb"

createPost :: [T.Text] -> UTCTime -> UTCTime -> Maybe Post
createPost postParam crtTime modTime
  | isNothing ty || isNothing ac = Nothing
  | otherwise = Just $ Post (procTitle $ postParam !! 0)
                (procCategories $ postParam !! 1)
                (postParam !! 2)
                crtTime
                modTime
                (fromJust ty)
                (fromJust ac)
  where ty = (textToInt $ postParam !! 3)
        ac = (textToInt $ postParam !! 4)
        procTitle "" = Nothing
        procTitle text = Just text
        procCategories "" = Nothing
        procCategories text = Just $ T.splitOn ", " text

createSystemVisit :: T.Text -> [T.Text] -> UTCTime -> Maybe SystemVisit
createSystemVisit author visitParam crtTime
  | isNothing reg || length sites /= length types = Nothing
  | otherwise = Just $ SystemVisit name (fromJust reg) (zip sites types)
                crtTime author
  where name = procName (visitParam !! 0)
        reg = procRegion (visitParam !! 1)
        sites = T.splitOn "," (visitParam !! 2)
        types = T.splitOn "," (visitParam !! 3)
        procRegion "" = Nothing
        procRegion text = Just text
        procName "" = Nothing
        procName text = Just text

dummyTime :: UTCTime
dummyTime = parseTimeOrError True defaultTimeLocale "%d.%m.%Y %H:%M"
            "01.01.2000 00:00"

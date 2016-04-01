{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}

module Web.Utils where

import qualified Data.Text as T
import Data.Text.Lazy (toStrict, fromStrict)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Web.Spock.Safe hiding (head, SessionId)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Text.Read
import Data.Maybe
import Database.Persist.Sqlite hiding (get)
import System.Log.Logger

import Model.Types
import Model.Model


{-
 Types and datas
-}
type SessionVal = Maybe SessionId
type BlogApp = SpockM SqlBackend SessionVal SiteConfig ()
type BlogAction a = SpockAction SqlBackend SessionVal SiteConfig a

data Route = DB T.Text Int | Redirect T.Text T.Text deriving Show
data SiteConfig = SiteConfig { rootDir :: T.Text
                             , database :: T.Text
                             , filesDir :: T.Text
                             , logDir :: T.Text
                             , routes :: [Route]
                             }
              deriving Show

{-
 Config parsing
-}
parseConfig :: T.Text -> SiteConfig
parseConfig t = SiteConfig rootDir db files logs routes
  where blocks = map T.lines $ T.splitOn "\n\n" t
        rootDir = foldl T.append "" $ map parseRootDir blocks
        db = foldl T.append "" $ map parseDatabase blocks
        files = foldl T.append "" $ map parseFilesDir blocks
        logs = foldl T.append "" $ map parseLogDir blocks
        rawRoutes = foldl (++) [] $ map parseRoutes blocks
        routes = map (constructRoute . tuplify3) rawRoutes

parseRootDir ("[RootDir]" : ts) = head $ ts
parseRootDir _ = ""

parseDatabase ("[Database]" : ts) = head $ ts
parseDatabase _ = ""

parseFilesDir ("[Files]" : ts) = head $ ts
parseFilesDir _ = ""

parseLogDir ("[Log]" : ts) = head $ ts
parseLogDir _ = ""

parseRoutes ("[Routes]" : ts) = map T.words ts
parseRoutes _ = []

constructRoute :: (T.Text, T.Text, T.Text) -> Route
constructRoute (url, "redirect", link) = Redirect url link
constructRoute (url, "db", pid) = DB url $ fromJust $ textToInt pid
constructRoute _ = undefined

{-
 Utility
-}
blaze :: MonadIO m => Html -> ActionT m a
blaze = html . toStrict . renderHtml

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) =>
          SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSQL action =
  runQuery $ \conn -> runResourceT $ runNoLoggingT $ runSqlConn action conn

-- Redirects to /login
reqLogin :: Maybe (UserId, User) -> BlogAction a -> BlogAction a
reqLogin Nothing _ = redirect "/login"
reqLogin _ action = action

-- Redirects to access denied (for GET)
reqRightPage :: Maybe (UserId, User) -> Int -> BlogAction a -> BlogAction a
reqRightPage Nothing reqAccess _ = do
  liftIO $ logAuth False reqAccess Nothing "page request"
  redirect "/login"
reqRightPage (Just (_, user)) reqAccess action =
  if checkUserRight user reqAccess
  then do
    liftIO $ logAuth True reqAccess (Just user) "page request"
    action
  else do
    liftIO $ logAuth False reqAccess (Just user) "page request"
    redirect "/accessDenied"

-- sends json error message (for POST)
reqRightPOST :: Maybe (UserId, User) -> Int -> BlogAction a -> BlogAction a
reqRightPOST Nothing reqAccess _ = do
  liftIO $ logAuth False reqAccess (Nothing) "POST"
  json ("post error: user not logged in" :: T.Text)
reqRightPOST (Just (_, user)) reqAccess action =
  if checkUserRight user reqAccess
  then do
    liftIO $ logAuth True reqAccess (Just user) "POST"
    action
  else do
    liftIO $ logAuth False reqAccess (Just user) "POST"
    json ("post error: user access denied" :: T.Text)

reqRightFile :: Maybe (UserId, User) -> Int -> BlogAction a -> BlogAction a
reqRightFile _ 0 action = action
reqRightFile (Just (_, user)) reqAccess action =
  if checkUserRight user reqAccess
  then do
    liftIO $ logAuth True reqAccess (Just user) "file request"
    action
  else do
    liftIO $ logAuth False reqAccess (Just user) "file request"
    text "Insufficient permission for file"
reqRightFile Nothing reqAccess _ = do
  liftIO $ logAuth False reqAccess (Nothing) "file request"
  text "Insufficient permission for file"

checkUserRight :: User -> Int -> Bool
checkUserRight user reqAccess = userAccess user >= reqAccess

loadUserSession :: BlogAction (Maybe (UserId, User))
loadUserSession = do
  sess <- readSession
  case sess of
   Nothing -> return Nothing
   Just sid -> do mUser <- runSQL $ queryUser sid
                  return mUser

{-
  General purpose
-}
textToInt :: T.Text -> Maybe Int
textToInt text = case decimal text of
                Left _ -> Nothing
                Right (val, "") -> Just val
                Right (_, _) -> Nothing

textToBool :: T.Text -> Maybe Bool
textToBool "True" = Just True
textToBool "False" = Just False
textToBool _ = Nothing

tuplify3 (a : b : c : []) = (a, b, c)
tuplify3 _ = undefined

-- first arg is what <- params returns, second the "targets"
findParam :: [(T.Text, T.Text)] -> T.Text -> Maybe T.Text
findParam [] _ = Nothing
findParam ((n, c) : xs) name | n == name = Just c
                             | otherwise = findParam xs name

findParams :: [(T.Text, T.Text)] -> [T.Text] -> [Maybe T.Text]
findParams xs names = map (findParam xs) names

logAuth :: Bool -> Int -> Maybe User -> String -> IO ()
logAuth False reqAccess muser action =
  warningM "auth" ("INVALID " ++ action ++  " at level=" ++
                   show reqAccess ++ " for " ++ convUser muser)
logAuth True reqAccess muser action =
  infoM "auth" ("Valid " ++ action ++ " at level=" ++
                show reqAccess ++ " for " ++ convUser muser)

convUser Nothing = "user=UNAUTH"
convUser (Just user) = "user=" ++ T.unpack (userName user)

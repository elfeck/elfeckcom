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
                             , routes :: [Route] }
              deriving Show

{-
 Config parsing
-}
parseConfig :: T.Text -> SiteConfig
parseConfig t = SiteConfig rootDir db files routes
  where blocks = map T.lines $ T.splitOn "\n\n" t
        rootDir = foldl T.append "" $ map parseRootDir blocks
        db = foldl T.append "" $ map parseDatabase blocks
        files = foldl T.append "" $ map parseFilesDir blocks
        rawRoutes = foldl (++) [] $ map parseRoutes blocks
        routes = map (constructRoute . tuplify3) rawRoutes

parseRootDir ("[RootDir]" : ts) = head $ ts
parseRootDir _ = ""

parseDatabase ("[Database]" : ts) = head $ ts
parseDatabase _ = ""

parseFilesDir ("[Files]" : ts) = head $ ts
parseFilesDir _ = ""

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
reqRightPage Nothing _ _ = redirect "/login"
reqRightPage (Just (_, user)) reqAccess action =
  if checkUserRight user reqAccess
  then action
  else redirect "/accessDenied"

-- sends json error message (for POST)
reqRightPOST :: Maybe (UserId, User) -> Int -> BlogAction a -> BlogAction a
reqRightPOST Nothing _ _ = json ("post error: user not logged in" :: T.Text)
reqRightPOST (Just (_, user)) reqAccess action =
  if checkUserRight user reqAccess
  then action
  else json ("post error: user access denied" :: T.Text)

reqRightFile :: Maybe (UserId, User) -> Int -> BlogAction a -> BlogAction a
reqRightFile _ 0 action = action
reqRightFile (Just (_, user)) reqAccess action =
  if checkUserRight user reqAccess
  then action
  else text "Insufficient permission for file"
reqRightFile _ _ _ = text "Insufficient permission for file"

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

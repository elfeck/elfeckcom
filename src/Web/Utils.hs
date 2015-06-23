{-# LANGUAGE OverloadedStrings #-}

module Web.Utils where

import qualified Data.Text as T
import Data.Text.Lazy (toStrict, fromStrict)
import Control.Monad.IO.Class
import Web.Spock.Safe hiding (head, SessionId)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Data.Text.Read
import Data.Maybe
import Database.Persist.Sqlite hiding (get)

import Model.Model

{-
 Types and datas
-}
type SessionVal = Maybe SessionId
type BlogApp = SpockM SqlBackend SessionVal SiteConfig ()
type BlogAction a = SpockAction SqlBackend SessionVal SiteConfig a

data Route = DB T.Text Int | Redirect T.Text T.Text deriving Show
data SiteConfig = SiteConfig { database :: T.Text
                             , routes :: [Route] }
              deriving Show

{-
 Config parsing
-}
parseConfig :: T.Text -> SiteConfig
parseConfig t = SiteConfig db routes
  where blocks = map T.lines $ T.splitOn "\n\n" t
        db = foldl T.append "" $ map parseDatabase blocks
        rawRoutes = foldl (++) [] $ map parseRoutes blocks
        routes = map (constructRoute . tuplify) rawRoutes

parseDatabase ("[Database]" : ts) = head $ ts
parseDatabase _ = ""

parseRoutes ("[Routes]" : ts) = map T.words ts
parseRoutes _ = []

constructRoute :: (T.Text, T.Text, T.Text) -> Route
constructRoute (url, "redirect", link) = Redirect url link
constructRoute (url, "db", pid) = DB url $ fromJust $ textToInt pid

{-
 Utility
-}
blaze :: MonadIO m => Html -> ActionT m a
blaze = html . toStrict . renderHtml

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

{-
  General purpose
-}
textToInt :: T.Text -> Maybe Int
textToInt text = case decimal text of
                Left _ -> Nothing
                Right (val, "") -> Just val
                Right (val, _) -> Nothing

tuplify (a : b : c : []) = (a, b, c)
tuplify _ = undefined

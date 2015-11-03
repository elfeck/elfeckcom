{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.GetHandler where

import qualified Data.Text as T
import Web.Spock.Safe hiding (head, SessionId)

import Data.List
import Data.Maybe
import System.Directory (getDirectoryContents)
import Control.Monad.IO.Class (liftIO)

import Web.View
import Web.Utils
import Web.PostParser
import Model.Types
import Model.Model


handleGets :: [Route] -> String -> BlogApp
handleGets staticRoutes rootDir = do
  sequence_ $ map handleStatic staticRoutes
  handleDrivel
  handleDrivelEntry
  handleLogin
  handleLogout
  handleEdit
  handleEvexpl
  handleWhyiliketrees rootDir
  handleUnknown

handleStatic :: Route -> BlogApp
handleStatic (Redirect from to) = get (static $ T.unpack from) $ redirect to
handleStatic (DB from pid) = get (static $ T.unpack from) $ do
  muser <- loadUserSession
  mpost <- runSQL $ queryPost pid
  case mpost of
   Nothing -> blaze $ do
     siteHead ""
     infBackHeader "invalid pid" (relPath from)
     siteBody siteInvPid
   Just post -> blaze $ do
     siteHead $ relPath from
     siteHeader $ relPath from
     genericBody (procURL from) $ parsePost post 0
     siteFooter (fmap snd muser) (Just $ snd post)
  where procURL "/" = "index"
        procURL "whyiliketrees" = "wilt"
        procURL u = T.unpack u

handleDrivel :: BlogApp
handleDrivel = get "drivel" $ do
  muser <- loadUserSession
  blaze $ do
    siteHead "./"
    siteHeader "./../"
    drivelBody
    siteFooter (fmap snd muser) Nothing

handleDrivelEntry :: BlogApp
handleDrivelEntry = get ("drivel" <//> "post" <//> var) $ \pid -> do
  muser <- loadUserSession
  mpost <- runSQL $ queryPost (pid :: Int)
  let access = if isNothing muser
               then 0
               else userAccess $ snd $ fromJust muser
  case mpost of
   Nothing -> error404 (fmap snd muser)
   Just post ->
     if postPtype (snd post) < 1
     then error404 (fmap snd muser)
     else
       if access < postAccess (snd post)
       then accessError (fmap snd muser)
       else blaze $ do
         siteHead "./../../"
         siteHeader "./../../"
         siteBody $ parsePost post 1
         siteFooter (fmap snd muser) Nothing

error404 user = blaze $ do siteHead $ "./../../"
                           infBackHeader "invalid url" "./../../"
                           siteBody $ site404
                           siteFooter user Nothing

accessError user = blaze $ do siteHead $ "./../../"
                              infBackHeader "access denied" "./../../"
                              siteBody $ siteAccessError
                              siteFooter user Nothing

handleEdit :: BlogApp
handleEdit = get "edit" $ do
  muser <- loadUserSession
  posts <- runSQL $ queryAllPosts
  reqRight muser 5 $ blaze $ do
    inputHead
    infBackHeader "edit" "./"
    siteEdit posts
    siteFooter (fmap snd muser) Nothing

handleLogin :: BlogApp
handleLogin = get "login" $ blaze $ do
  inputHead
  infBackHeader "login" "./"
  siteLogin

handleLogout :: BlogApp
handleLogout = get "logout" $ do
  muser <- loadUserSession
  case muser of
   Nothing -> redirect "/whyiliketrees/"
   Just (userId, _) -> do
     runSQL $ logoutUser userId
     writeSession Nothing
     redirect "/"

handleEvexpl :: BlogApp
handleEvexpl = get "evexpl" $ do
  muser <- loadUserSession
  visits <- runSQL $ queryAllVisits
  reqRight muser 5 $ blaze $ do
    inputHead
    infBackHeader "evexpl" "./"
    siteEvexpl visits
    siteFooter (fmap snd muser) Nothing

handleWhyiliketrees :: String -> BlogApp
handleWhyiliketrees rootDir = get "games/whyiliketrees" $ do
  gameFiles <- liftIO $ getDirectoryContents
               (rootDir ++ "/static/games/whyiliketrees/")
  muser <- loadUserSession
  blaze $ do
    siteHead  "../"
    darkHeader "../"
    whyiliketreesBody (filter onlyJs gameFiles)
    siteFooter (fmap snd muser) Nothing

handleUnknown :: BlogApp
handleUnknown = hookAny GET $ \path -> do
  muser <- loadUserSession
  let from = foldl T.append "" (intersperse "/" path)
  blaze $ do
    siteHead $ relPath from
    infBackHeader "invalid url" (relPath from)
    siteBody $ site404
    siteFooter (fmap snd muser) Nothing

relPath :: T.Text -> String
relPath from = "./" ++ foldl (++) "" (replicate d "../")
  where d = length $ [f | f <- T.unpack from, f == '/']

onlyJs :: String -> Bool
onlyJs xs = "sj." == take 3 (reverse xs)

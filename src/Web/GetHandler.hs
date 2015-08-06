{-# LANGUAGE OverloadedStrings #-}

module Web.GetHandler where

import qualified Data.Text as T
import Web.Spock.Safe hiding (head, SessionId)

import System.Directory (getDirectoryContents)
import Control.Monad.IO.Class (liftIO)

import Web.View
import Web.Utils
import Web.PostParser
import Model.Model


handleGets :: [Route] -> String -> BlogApp
handleGets staticRoutes rootDir = do
  sequence_ $ map handleStatic staticRoutes
  get "edit" $ do
    muser <- loadUserSession
    posts <- runSQL $ queryAllPosts
    reqRight muser 5 $ blaze $ do
      inputHead
      infBackHeader "edit"
      siteEdit posts
      siteFooter $ fmap snd muser
  get "login" $ blaze $ do
    inputHead
    infBackHeader "login"
    siteLogin
  get "logout" $ do
    muser <- loadUserSession
    case muser of
     Nothing -> redirect "/whyiliketrees/"
     Just (userId, _) -> do
       runSQL $ logoutUser userId
       writeSession Nothing
       redirect "/"
  get "evexpl" $ do
    muser <- loadUserSession
    visits <- runSQL $ queryAllVisits
    reqRight muser 5 $ blaze $ do
      inputHead
      infBackHeader "evexpl"
      siteEvexpl visits
      siteFooter $ fmap snd muser
  get "games/whyiliketrees" $ do
    gameFiles <- liftIO $ getDirectoryContents
                 (rootDir ++ "/static/games/whyiliketrees/")
    muser <- loadUserSession
    blaze $ do
      siteHead  "../"
      darkHeader "../"
      whyiliketreesBody (filter onlyJs gameFiles)
      siteFooter $ fmap snd muser
  hookAny GET $ \_ -> blaze $ do
    siteHead ""
    emptyHeader ""
    site404

onlyJs :: String -> Bool
onlyJs xs = "sj." == take 3 (reverse xs)

handleStatic :: Route -> BlogApp
handleStatic (Redirect from to) = get (static $ T.unpack from) $ redirect to
handleStatic (DB from pid) = get (static $ T.unpack from) $ do
  muser <- loadUserSession
  mpost <- runSQL $ queryPost pid
  case mpost of
   Nothing -> handleInvPid
   Just post -> blaze $ do
     siteHead ""
     siteHeader ""
     genericBody (procURL from) $ parsePost $ snd post
     siteFooter $ fmap snd muser
       where procURL "/" = "index"
             procURL "whyiliketrees" = "wilt"
             procURL u = T.unpack u

handleInvPid = blaze $ do
  siteHead ""
  siteHeader ""
  siteInvPid

{-# LANGUAGE OverloadedStrings #-}

module Web.GetHandler where

import qualified Data.Text as T
import Web.Spock.Safe hiding (head, SessionId)

import Web.View
import Web.Utils
import Web.PostParser
import Model.Model


handleGets :: [Route] -> BlogApp
handleGets staticRoutes = do
  sequence_ $ map handleStatic staticRoutes
  get "edit" $ do
    muser <- loadUserSession
    posts <- runSQL $ queryAllPosts
    reqRight muser 5 $ blaze $ do
      siteHead
      infBackHeader "edit"
      siteEdit posts
  get "login" $ blaze $ do
    siteHead
    infBackHeader "login"
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
    reqRight muser 5 $ blaze $ do
      siteHead
      infBackHeader "evexpl"
      siteEvexpl visits
  hookAny GET $ \_ -> blaze $ do
    siteHead
    emptyHeader
    site404

handleStatic :: Route -> BlogApp
handleStatic (Redirect from to) = get (static $ T.unpack from) $ redirect to
handleStatic (DB from pid) = get (static $ T.unpack from) $ do
  muser <- loadUserSession
  mpost <- runSQL $ queryPost pid
  case mpost of
   Nothing -> handleInvPid
   Just post -> blaze $ do
     siteHead
     siteHeader
     siteBody $ parsePost $ snd post
     siteFooter $ fmap snd muser

handleInvPid = blaze $ do
  siteHead
  siteHeader
  siteInvPid

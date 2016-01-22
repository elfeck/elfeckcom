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
import Web.FileHandler
import Model.Types
import Model.Model


handleGets :: [Route] -> String -> String -> BlogApp
handleGets staticRoutes rootDir filesDir = do
  handleUploads filesDir
  sequence_ $ map handleStaticSite staticRoutes
  handleDrivel
  handleDrivelEntry
  handleLogin
  handleLogout
  handleEdit
  handleUpload
  handleWhyiliketrees rootDir
  handleLD29
  handleUnknown

-- TODO: Content Type!!
-- TODO: send 404 error and also in Utils ~ send access HTML error
-- TODO: SUPER UGLY
handleUploads :: String -> BlogApp
handleUploads filesDir = subcomponent "uploads" $ do
  get (var <//> var) $ \perm fileName -> handl perm fileName
  get (var <//> var <//> var) $ \perm fn1 fn2 -> handl perm
                                                 (fn1 ++ "/" ++ fn2)
  get (var <//> var <//> var <//> var) $ \perm fn1 fn2 fn3 -> handl perm
                                                              (fn1 ++ "/" ++
                                                               fn2 ++ "/" ++
                                                               fn3)
    where handl perm fileName = do
            muser <- loadUserSession
            reqRightFile muser perm $ do
              let filePath = concat ["/upload/access/",
                                     show perm, "/", fileName]
              mfilep <- liftIO $ checkFile filesDir filePath
              case mfilep of
                Just path -> do file "" path
                Nothing -> do text "requested file not found"

handleStaticSite :: Route -> BlogApp
handleStaticSite (Redirect from to) = get (static $ T.unpack from) $
                                      redirect to
handleStaticSite (DB from pid) = get (static $ T.unpack from) $ do
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
     genericBody (procURL from) $ parsePost (snd post) 0
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
    drivelBody (fmap snd muser)
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
   Just post -> do
     -- prevent serving statically handled sites (should never happen but ok)
     if postPtype (snd post) < 1
     then error404 (fmap snd muser)
     else
       if access < postAccess (snd post)
       then accessError (fmap snd muser)
       else blaze $ do
         siteHead "./../../"
         siteHeader "./../../"
         siteBody $ parsePost (snd post) (postPtype $ snd post)
         katex "./../../"
         siteFooter (fmap snd muser) (Just $ snd post)

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
  reqRightPage muser 5 $ blaze $ do
    inputHead
    minimalHeader "edit" "./"
    siteEdit posts
    siteFooter (fmap snd muser) Nothing

handleUpload :: BlogApp
handleUpload = get "upload" $ do
  muser <- loadUserSession
  --reqRightPage muser 5 $ blaze $ do
  blaze $ do
    inputHead
    infBackHeader "upload" "./"
    siteUpload
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

handleLD29 :: BlogApp
handleLD29 = do
  get "games/LD29" $ do
    blaze $ do
      siteHead "../"
      --darkHeader "../"
      ld29Body
      --siteFooter (fmap snd muser) Nothing
  get "games/letterMap.png" $ redirect "/static/games/LD29/letterMap.png"

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

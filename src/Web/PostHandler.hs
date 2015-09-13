{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

module Web.PostHandler where

import qualified Data.Text as T
import Web.Spock.Safe hiding (head, SessionId)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe


import Web.Utils
import Web.PostParser
import Web.JsonParser
import Model.Model
import Model.Types


handlePosts :: BlogApp
handlePosts = do
  handleDrivelCategories
  handleDrivelPosts
  handleEditPreview
  handleEditSubmit
  handleEditLoad
  handleEvexplSubmit
  handleEvexplLoad
  handleLoginSubmit

handleDrivelCategories :: BlogApp
handleDrivelCategories = post "drivel/categories" $ do
  muser <- loadUserSession
  let access = if isNothing muser
               then 0
               else userAccess (snd $ fromJust muser)
  cats <- runSQL $ queryAllDrivelCategories access
  json cats

handleDrivelPosts :: BlogApp
handleDrivelPosts = post "drivel/posts" $ do
  muser <- loadUserSession
  let access = if isNothing muser
               then 0
               else userAccess (snd $ fromJust muser)
  dat <- params
  let mrawp = sequence $ findParams dat ["from", "till", "cats", "postOnly"]
  case processParams mrawp of
   Just (from, till, cats, ponly) -> do
     posts <- runSQL $ queryDrivel access (from, till) cats ponly
     getpostsResponse $ fmap snd posts
   Nothing -> errorJson
  where processParams Nothing = Nothing
        processParams (Just [f, t, c, p]) =
          case (textToInt f, textToInt t, textToBool p) of
           (Just mf, Just mt, Just mp) -> Just (mf, mt, procCats c, mp)
           _ -> Nothing
           where procCats "" = []
                 procCats cats = T.splitOn "," cats
        processParams _ = Nothing


handleEditPreview :: BlogApp
handleEditPreview = post "edit/preview" $ do
  muser <- loadUserSession
  reqRight' muser 5 $ do
    dat <- params
    let mpost = jsonToPost dat
    case (mpost) of
     Just post -> previewResponse post
     Nothing -> errorJson

handleEditSubmit :: BlogApp
handleEditSubmit = post "edit/submit" $ do
  muser <- loadUserSession
  reqRight' muser 5 $ do
    dat <- params
    let msubmitType = findParam dat "submitType"
    let mpid = fmap textToInt $ findParam dat "pid"
    let mpost = jsonToPost dat
    case (msubmitType, mpid, mpost) of
     (Just st, Just (Just pid), Just post) -> submitEdit st pid post
     _ -> errorJson

handleEditLoad :: BlogApp
handleEditLoad = post "edit/loadpost" $ do
  muser <- loadUserSession
  reqRight' muser 5 $ do
    dat <- params
    let mpid = fmap textToInt $ findParam dat "pid"
    case mpid of
     Nothing -> errorJson
     Just (Just pid) -> do
       resp <- runSQL $ queryPost pid
       loadpostResponse resp

handleEvexplSubmit :: BlogApp
handleEvexplSubmit = post "evexpl/submit" $ do
  muser <- loadUserSession
  reqRight' muser 5 $ do
    dat <- params
    let msubmitType = findParam dat "submitType"
    let meid = fmap textToInt $ findParam dat "eid"
    let mvisit = jsonToSystemVisit (fmap snd muser) dat
    case (msubmitType, meid, mvisit) of
     (Just st, Just (Just (eid)), Just visit) -> submitEvexpl st eid visit
     _ -> json $ (show mvisit)

handleEvexplLoad :: BlogApp
handleEvexplLoad = post "/evexpl/loadvisit" $ do
  muser <- loadUserSession
  reqRight' muser 5 $ do
    dat <- params
    let meid = fmap textToInt $ findParam dat "eid"
    case meid of
     Nothing -> errorJson
     Just (Just eid) -> do
       resp <- runSQL $ queryVisit eid
       loadvisitResponse resp

handleLoginSubmit :: BlogApp
handleLoginSubmit = post "login/submit" $ do
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

getpostsResponse posts = json $ map (\p -> renderPost p 2) posts

previewResponse post = json $ renderPost post 0

loginResponse True =  json (("login success. yey" :: T.Text), True)
loginResponse False =  json (("wrong login data, try again" :: T.Text), False)

submitResponse resp = json resp

loadpostResponse Nothing = json ("could not find post to id" :: T.Text)
loadpostResponse (Just post) = json $ post

loadvisitResponse Nothing = json ("could not find visit to id" :: T.Text)
loadvisitResponse (Just visit) = json $ visit

errorJson = json $ ("ney: json invld" :: T.Text)

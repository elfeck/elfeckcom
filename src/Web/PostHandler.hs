{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

module Web.PostHandler where

import qualified Data.Text as T
import Web.Spock.Safe hiding (head, SessionId)
import Data.Maybe
import Data.Time

import Web.Utils
import Web.PostParser
import Web.JsonParser
import Model.Model
import Model.Types


handlePosts :: BlogApp
handlePosts = do
  post "edit/preview" $ do
    muser <- loadUserSession
    reqRight' muser 5 $ do
      dat <- params
      let mpost = jsonToPost dat
      case (mpost) of
       Just post -> previewResponse post
       Nothing -> errorJson
  post "edit/submit" $ do
    muser <- loadUserSession
    reqRight' muser 5 $ do
      dat <- params
      let msubmitType = findParam dat "submitType"
      let mpid = fmap textToInt $ findParam dat "pid"
      let mpost = jsonToPost dat
      case (msubmitType, mpid, mpost) of
       (Just st, Just (Just pid), Just post) -> submitEdit st pid post
       _ -> errorJson
  post "edit/loadpost" $ do
    muser <- loadUserSession
    reqRight' muser 5 $ do
      dat <- params
      let mpid = fmap textToInt $ findParam dat "pid"
      case mpid of
       Nothing -> errorJson
       Just (Just pid) -> do
         resp <- runSQL $ queryPost pid
         loadpostResponse resp
  post "evexpl/submit" $ do
    muser <- loadUserSession
    reqRight' muser 5 $ do
      dat <- params
      let msubmitType = findParam dat "submitType"
      let meid = fmap textToInt $ findParam dat "eid"
      let mvisit = jsonToSystemVisit (fmap snd muser) dat
      case (msubmitType, meid, mvisit) of
       (Just st, Just (Just (eid)), Just visit) -> submitEvexpl st eid visit
       _ -> json $ (show mvisit)
  post "/evexpl/loadvisit" $ do
    muser <- loadUserSession
    reqRight' muser 5 $ do
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

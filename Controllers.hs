{-# LANGUAGE OverloadedStrings #-}

module Controllers where

import Web.Scotty (ScottyM, ActionM, get, post, notFound, html, redirect,
                   param, rescue, json)
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import View


home :: String -> ScottyM()
home svg = get "/" $ blaze $ do
  siteHead
  siteHeader svg True
  testBody

elfeck :: ScottyM()
elfeck = get "/elfeck" $ redirect "/"

edit :: String -> ScottyM()
edit svg = get "/edit" $ blaze $ do
  siteHead
  siteHeader svg False
  siteEdit

editsubmit :: ScottyM()
editsubmit = post "/edit/submit" $ do
  editContent <- param "content" `rescue` (\msg -> return msg)
  json ("okay" :: String)

error404 :: String -> ScottyM()
error404 svg = notFound $ blaze $ do
  siteHead
  siteHeader svg True
  site404

blaze :: Html -> ActionM()
blaze = html . renderHtml

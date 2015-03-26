{-# LANGUAGE OverloadedStrings #-}

module Controllers (home, elfeck, error404) where

import Web.Scotty (ScottyM, ActionM, get, notFound, html, redirect)
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import View


home :: String -> ScottyM()
home svg = get "/" $ blaze $ do
  siteHead
  siteHeader svg
  testBody

elfeck :: ScottyM()
elfeck = get "/elfeck" $ redirect "/"

error404 :: String -> ScottyM()
error404 svg = notFound $ blaze $ do
  siteHead
  site404

blaze :: Html -> ActionM()
blaze = html . renderHtml

{-# LANGUAGE OverloadedStrings #-}

module Controllers (home, error404) where

import Web.Scotty (ScottyM, ActionM, get, notFound, html)
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import View


home :: ScottyM()
home = get "/" $ blaze site

error404 :: ScottyM()
error404 = notFound $ blaze site404


blaze :: Html -> ActionM()
blaze = html . renderHtml

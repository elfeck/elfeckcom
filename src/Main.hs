{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Text.Lazy (toStrict)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Web.Spock.Safe hiding (head)

import View


main :: IO ()
main = do
  svg <- readFile "static/header.svg"
  let files = [svg]
  runSpock 3000 $ spockT id $ app files

app :: MonadIO m => [String] -> SpockT m ()
app files = do
  middleware (staticPolicy (addBase "static"))
  handleRoutes files

handleRoutes :: MonadIO m => [String] -> SpockT m ()
handleRoutes files = do
  get root $ blaze $ do
    siteHead
    siteHeader (head files)
    testBody
  get "edit" $ blaze $ do
    siteHead
    infBackHeader (head files) "edit"
    siteEdit
  get "login" $ blaze $ do
    siteHead
    infBackHeader (head files) "login"
    siteLogin
  hookAny GET $ \_ -> blaze $ do
    siteHead
    emptyHeader (head files)
    site404

blaze :: MonadIO m => Html -> ActionT m a
blaze = html . toStrict . renderHtml

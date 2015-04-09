{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Text.Lazy (toStrict, fromStrict)
import qualified Data.Text as T
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Web.Spock.Safe hiding (head)
import Data.Maybe

import View
import EntryParser

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
  handlePosts
  hookAny GET $ \_ -> blaze $ do
    siteHead
    emptyHeader (head files)
    site404


handlePosts :: (MonadIO m) => SpockT m ()
handlePosts = do
  post "edit/submit" $ do
    dat <- params
    let title = findParam dat "dat[title]"
    let cats = findParam dat "dat[categories]"
    let content = findParam dat "dat[content]"
    editResponse [title, cats, content]
  post "login/submit" $ do
    dat <- params
    let name = findParam dat "dat[name]"
    let pass = findParam dat "dat[pass]"
    loginResponse [name, pass]

editResponse xs
  | null $ filter isNothing xs =
      json $ parseEdit $ (map (fromStrict . fromJust) xs)
  | otherwise = json $ ("error in sent json" :: T.Text)

loginResponse xs
  | null $ filter isNothing xs = case tryLogin (map fromJust xs) of
    Nothing -> json $ ("invalid login. sorry" :: T.Text)
    Just uid -> undefined
  | otherwise = json $ ("error in sent json" :: T.Text)

findParam :: [(T.Text, T.Text)] -> T.Text -> Maybe T.Text
findParam [] _ = Nothing
findParam ((n, c) : xs) name | n == name = Just c
                             | otherwise = findParam xs name

tryLogin :: [T.Text] -> Maybe Int
tryLogin xs | length xs /= 2 = Nothing
            | otherwise = Nothing

blaze :: MonadIO m => Html -> ActionT m a
blaze = html . toStrict . renderHtml

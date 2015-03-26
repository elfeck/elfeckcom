{-# LANGUAGE OverloadedStrings #-}

module View (siteHead, siteHeader, site404, testBody) where

import Prelude hiding (div, head, id)
import Text.Blaze.Html (preEscapedString, string, stringValue)
import Text.Blaze.Html5 (Html, (!), docTypeHtml,
                         head, meta, title, link,
                         body, div, img, ul, li, a)
import Text.Blaze.Html5.Attributes (charset,
                                    href, rel, src, type_, class_)


siteHead :: Html
siteHead = docTypeHtml $ do
  head $ do
    title "elfeck"
    meta ! charset "utf-8"
    link ! href "/index.css" ! rel "stylesheet" ! type_ "text/css"

siteHeader :: String -> Html
siteHeader headerSvg = do
  div ! class_ "header" $ do
    preEscapedString headerSvg
    ul ! class_ "headerleft" $ do
      headerEntry "elfeck"
      headerEntry "whyiliketrees"
    ul ! class_ "headerright" $ do
      headerEntry "math and stuff"
      headerEntry "drivel"

headerEntry :: String -> Html
headerEntry name = do
  li ! class_ "headerentry" $ do
    a (string name) ! href (stringValue ("/" ++ [a | a <- name, a /= ' ']))
      ! class_ "headerlink"

testBody :: Html
testBody = div "" ! (class_ "testbody")

site404 :: Html
site404 = do
  body $ do
    div "Sorry nothing to see here. Try a different URL"

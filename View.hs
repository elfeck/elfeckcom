{-# LANGUAGE OverloadedStrings #-}

module View (site, site404) where

import Prelude hiding (div, head, id)
import Text.Blaze.Html5 (Html, (!), docTypeHtml,
                         head, meta, title, link,
                         body, div)
import Text.Blaze.Html5.Attributes (charset,
                                    href, rel, type_)


siteHead :: Html
siteHead = docTypeHtml $ do
  head $ do
    title "elfeck"
    meta ! charset "utf-8"
    link ! href "/index.css" ! rel "stylesheet" ! type_ "text/css"

site :: Html
site = do
  siteHead
  body $ do
    div "Hello. This all is new and in haskell"

site404 :: Html
site404 = do
  siteHead
  body $ do
    div "Sorry nothing to see here. Try a different URL"

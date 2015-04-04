{-# LANGUAGE OverloadedStrings #-}

module View where

import Prelude hiding (div, head, id)
import Text.Blaze.Html (preEscapedString, string, stringValue)
import Text.Blaze.Html5 (Html, (!), docTypeHtml,
                         head, meta, title, link, script,
                         body, div, img, ul, li, a,
                         textarea, input)
import Text.Blaze.Html5.Attributes (charset,
                                    href, rel, src, type_, class_, id)


siteHead :: Html
siteHead = docTypeHtml $ do
  head $ do
    title "elfeck"
    meta ! charset "utf-8"
    link ! href "/index.css" ! rel "stylesheet" ! type_ "text/css"
    link ! href "/edit.css" ! rel "stylesheet" ! type_ "text/css"
    link ! href "/icon.png" ! rel "icon" ! type_ "image/png"

siteHeader :: String -> Bool -> Html
siteHeader headerSvg True = do
  div ! class_ "header" $ do
    div ! class_ "headercontainer" $ do
      ul ! class_ "headerleft" $ do
        headerEntry "elfeck"
        headerEntry "whyiliketrees"
    div ! class_ "headercontainer" $ do
      preEscapedString headerSvg
    div ! class_ "headercontainer" $ do
      ul ! class_ "headerright" $ do
        headerEntry "math and stuff"
        headerEntry "drivel"
siteHeader headerSvg False = do
  div ! class_ "header" $ do
    div ! class_ "headercontainer" $ do
      ul "" ! class_ "headerleft"
    div ! class_ "headercontainer" $ do
      preEscapedString headerSvg
    div ! class_ "headercontainer" $ do
      ul "" ! class_ "headerright"

headerEntry :: String -> Html
headerEntry name = do
  li ! class_ "headerentry" $ do
    a (string name) ! href (stringValue ("/" ++ [a | a <- name, a /= ' ']))
      ! class_ "headerlink"

testBody :: Html
testBody = div "" ! class_ "testbody"

site404 :: Html
site404 = do
  div "Sorry nothing to see here. Try a different URL" ! class_ "testbody"

siteEdit :: Html
siteEdit = do
  script "" ! src "/jquery-2.1.3.min.js"
  script "" ! src "/edit.js"
  div ! class_ "editbody" $ do
    input ! class_ "editin" ! id "edittitle"
    input ! class_ "editin" ! id "editcategories"
    textarea "" ! id "editarea"
    div "" ! id "editpreview"

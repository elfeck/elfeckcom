{-# LANGUAGE OverloadedStrings #-}

module View where

import Prelude hiding (div, head, id)
import Text.Blaze.Html (preEscapedString, string, stringValue, toHtml)
import Text.Blaze.Html5 (Html, (!), docTypeHtml,
                         head, meta, title, link, script,
                         body, div, img, ul, li, a,
                         textarea, input)
import Text.Blaze.Html5.Attributes (charset,
                                    href, rel, src, type_, class_, id, style)

siteHead :: Html
siteHead = docTypeHtml $ head $ do
  title "elfeck"
  meta ! charset "utf-8"
  link ! href "/index.css" ! rel "stylesheet" ! type_ "text/css"
  link ! href "/edit.css" ! rel "stylesheet" ! type_ "text/css"
  link ! href "/login.css" ! rel "stylesheet" ! type_ "text/css"
  link ! href "/icon.png" ! rel "icon" ! type_ "image/png"

siteHeader :: String -> Html
siteHeader headerSvg = do
  div ! class_ "header" $ do
    div ! class_ "headercontainer" $ ul ! class_ "headerleft" $ do
      headerEntry "elfeck"
      headerEntry "whyiliketrees"
    div ! class_ "headercontainer" $ preEscapedString headerSvg
    div ! class_ "headercontainer" $ ul ! class_ "headerright" $ do
      headerEntry "math and stuff"
      headerEntry "drivel"

emptyHeader :: String -> Html
emptyHeader headerSvg = do
  div ! class_ "header" $ do
    div ! class_ "headercontainer" $ ul "" ! class_ "headerleft"
    div ! class_ "headercontainer" $ preEscapedString headerSvg
    div ! class_ "headercontainer" $ ul "" ! class_ "headerright"

infBackHeader :: String -> String -> Html
infBackHeader headerSvg inf = do
  div ! class_ "header" $ do
    div ! class_ "headercontainer" $ ul ! class_ "headerleft" $ do
      li ! class_ "headerentry" $ toHtml inf
      li ! class_ "headerentry" $ ""
    div ! class_ "headercontainer" $ preEscapedString headerSvg
    div ! class_ "headercontainer" $ ul ! class_ "headerright" $ do
      li ! class_ "headerentry" $ ""
      li ! class_ "headerentry" $ a "back" ! href "/" ! class_ "headerlink"


headerEntry :: String -> Html
headerEntry name =
  li ! class_ "headerentry" $
  a (toHtml name) ! href (stringValue ("/" ++ [a | a <- name, a /= ' '])) !
  class_ "headerlink"

testBody :: Html
testBody = div "" ! class_ "testbody"

site404 :: Html
site404 = div "Sorry nothing to see here" ! class_ "testbody"

siteEdit :: Html
siteEdit = do
  script "" ! src "/jquery-2.1.3.min.js"
  script "" ! src "/edit.js"
  div ! class_ "editbody" $ do
    input ! class_ "editin" ! id "edittitle"
    input ! class_ "editin" ! id "editcategories"
    textarea "" ! id "editarea"
    div "" ! id "editpreview"

siteLogin :: Html
siteLogin = do
  script "" ! src "/jquery-2.1.3.min.js"
  script "" ! src "/edit.js"
  div ! class_ "logbody" $ do
    div "name ~"! class_ "loginfo"
    input ! class_ "login" ! id "logname" ! type_ "text"
    div "" ! class_ "loginspacer"
    div "pass ~"! class_ "loginfo"
    input ! class_ "login" ! id "logpw" ! type_ "password"
    div "login" ! id "logbut"

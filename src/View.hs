{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module View where

import Prelude hiding (div, head, id)
import Data.Maybe
import Data.Time.Format
import qualified Data.Text as T
import Text.Blaze.Html (preEscapedString, string, stringValue, toHtml)
import Text.Blaze.Html5 (Html, (!), docTypeHtml,
                         head, meta, title, link, script,
                         body, div, img, ul, li, a,
                         textarea, input, select, option)
import Text.Blaze.Html5.Attributes (charset,
                                    href, rel, src, type_, class_, id, style,
                                    multiple, readonly, selected,
                                    autocomplete)
import Database.Persist.Sql

import Model

siteHead :: Html
siteHead = docTypeHtml $ head $ do
  title "elfeck"
  meta ! charset "utf-8"
  link ! href "/css/index.css" ! rel "stylesheet" ! type_ "text/css"
  link ! href "/css/edit.css" ! rel "stylesheet" ! type_ "text/css"
  link ! href "/css/login.css" ! rel "stylesheet" ! type_ "text/css"
  link ! href "/img/icon.png" ! rel "icon" ! type_ "image/png"
  link ! href "http://fonts.googleapis.com/css?family=Open+Sans|Crimson+Text"
    ! rel "stylesheet" ! type_ "text/css"

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
      li ! class_ "headerentry" $ a "home" ! href "/" ! class_ "headerlink"

headerEntry :: String -> Html
headerEntry name =
  li ! class_ "headerentry" $
  a (toHtml name) ! href (stringValue ("/" ++ [a | a <- name, a /= ' '])) !
  class_ "headerlink"

siteFooter :: Maybe User -> Html
siteFooter muser = do
  div ! class_ "footer" $ do
    div ! class_ "userinfo" $ "["
    wrapContainer $ do
      div "built with" ! class_ "footerinfo"
      a " Spock " ! class_ "footerlink" ! href "http://www.spock.li/"
      div "& written in haskell" ! class_ "footerinfo"
    div ! class_ "usersep" $ "|"
    wrapContainer $ a "impressum" ! class_ "footerlink" ! href "/impressum"
    if isNothing muser
      then do div ! class_ "usersep" $ "|"
              wrapContainer $ a "login" ! class_ "footerlink" ! href "/login"
              div ! class_ "userinfo" $ "]"
      else do div ! class_ "userinfo " $ "]"
              spacer
    case muser of
     Nothing -> return ()
     Just user -> do
       wrapContainer $ a "edit" ! class_ "userlink" ! href "/edit"
       div ! class_ "usersep" $ "|"
       wrapContainer $ a "users" ! class_ "userlink" ! href "/manage"
       div ! class_ "usersep" $ "|"
       wrapContainer $ a "evexpl" ! class_ "userlink" ! href "/evexpl"
       div ! class_ "usersep" $ "|"
       wrapContainer $ a "logout" ! class_ "userlink" ! href "/logout"
       div ! class_ "userinfo" $ "]"

wrapContainer a = div ! class_ "footercont" $ a
spacer = div "[" ! class_ "footerspacer"

testBody :: Html
testBody = div "" ! class_ "testbody"

site404 :: Html
site404 = div "Sorry nothing to see here" ! class_ "testbody"

siteEdit :: [(PostId, Post)] -> Html
siteEdit posts = do
  script "" ! src "/js/jquery-2.1.3.min.js"
  script "" ! src "/js/button.js"
  script "" ! src "/js/edit.js"
  div ! class_ "editbody" $ do
    div ! class_ "editleft" $ do
      input ! class_ "editin" ! id "edittitle"
      input ! class_ "editin" ! id "editcategories"
      input ! class_ "editin" ! id "edittype"
      input ! class_ "editin" ! id "editaccess"
      textarea "" ! id "editarea"
    div ! class_ "editright" $ do
      input ! class_ "editin" ! id "editid" ! readonly "readonly"
      input ! class_ "editin" ! id "editdc" ! readonly "readonly"
      select ! class_ "editselect" ! multiple "multiple" ! id "editlist"
        ! autocomplete "off" $ do
          option ! id "0" ! selected "selected" $ "[new post]"
          toHtml $ map postToSelect posts
      div ! id "editresp" ! class_ "editinfo" $ ""
      input ! class_ "editin" ! id "editdelete"
      div ! id "submitbutton" ! class_ "button buttonidle" $ "S"
    div "" ! id "editpreview"

postToSelect :: (PostId, Post) -> Html
postToSelect (pid, post) = case postTitle post of
  Nothing -> option ! id (keyToId pid) $
             toHtml (T.concat ["[post from ", form (postModDate post), "]"])
  Just title -> option ! id (keyToId pid) $ toHtml title
  where form date = T.pack $ formatTime defaultTimeLocale "%d. %b %R" date
        keyToId pid = (stringValue (show $ fromSqlKey pid))

siteLogin :: Html
siteLogin = do
  script "" ! src "/js/jquery-2.1.3.min.js"
  script "" ! src "/js/button.js"
  script "" ! src "/js/login.js"
  div ! class_ "logbody" $ do
    div "name"! class_ "loginfo"
    input ! class_ "login" ! id "logname" ! type_ "text"
    div "" ! class_ "loginspacer"
    div "pass"! class_ "loginfo"
    input ! class_ "login" ! id "logpw" ! type_ "password"
    div ! class_ "logrespcont" $ do
      div "login" ! id "logbut" ! class_ "button buttonidle"
      div "" ! id "logresponse"

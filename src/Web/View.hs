{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Web.View where

import Prelude hiding (div, head, id)
import Data.Maybe
import Data.Time.Format
import qualified Data.Text as T
import Database.Persist.Sql (fromSqlKey)
import Text.Blaze.Html (preEscapedString, string, stringValue, toHtml)
import Text.Blaze.Html5 (Html, (!), docTypeHtml, head, meta, title, link,
                         script, body, div, img, ul, li, a, textarea, input,
                         select, option, br, canvas)
import Text.Blaze.Html5.Attributes (charset, href, rel, src, type_, class_,
                                    id, style, multiple, readonly, selected,
                                    autocomplete)

import Model.Types


{-
 Index Elements: Head, Header and Footer
 Sadly all Pages use the same Head so all CSS is required right here
-}
siteHead :: String -> Html
siteHead path = docTypeHtml $ head $ do
  title "elfeck"
  meta ! charset "utf-8"
  link ! href (appPath "static/css/index.css") ! rel "stylesheet"
    ! type_ "text/css"
  link ! href (appPath "static/css/site.css") ! rel "stylesheet"
    ! type_ "text/css"
  link ! href (appPath "static/img/icon.png") ! rel "icon"
    ! type_ "image/png"
  link ! href "http://fonts.googleapis.com/css?family=Open+Sans|Crimson+Text"
    ! rel "stylesheet" ! type_ "text/css"
    where appPath p = stringValue (path ++ p)

-- Missing root-path adjustment
inputHead :: Html
inputHead = docTypeHtml $ head $ do
  title "elfeck"
  meta ! charset "utf-8"
  link ! href "static/css/index.css" ! rel "stylesheet" ! type_ "text/css"
  link ! href "static/css/input.css" ! rel "stylesheet" ! type_ "text/css"
  link ! href "static/css/edit.css" ! rel "stylesheet" ! type_ "text/css"
  link ! href "static/css/login.css" ! rel "stylesheet" ! type_ "text/css"
  link ! href "static/css/evexpl.css" ! rel "stylesheet" ! type_ "text/css"
  link ! href "static/css/site.css" ! rel "stylesheet" ! type_ "text/css"
  link ! href "static/css/lib/jquery-ui.min.css" ! rel "stylesheet"
    ! type_ "text/css"
  link ! href "static/img/icon.png" ! rel "icon" ! type_ "image/png"
  link ! href "http://fonts.googleapis.com/css?family=Open+Sans|Crimson+Text"
    ! rel "stylesheet" ! type_ "text/css"


siteHeader :: String -> Html
siteHeader path = do
  div ! class_ "header" $ do
    div ! class_ "headercontainer" $ ul ! class_ "headerleft" $ do
      headerEntry "elfeck"
      headerEntry "whyiliketrees"
    div ! class_ "headercontainer" $ img
      ! src (stringValue $ path ++ "static/img/header.svg")
    div ! class_ "headercontainer" $ ul ! class_ "headerright" $ do
      headerEntry "math and stuff"
      headerEntry "drivel"

emptyHeader :: String -> Html
emptyHeader path = do
  div ! class_ "header" $ do
    div ! class_ "headercontainer" $ ul "" ! class_ "headerleft"
    div ! class_ "headercontainer" $ img
      ! src (stringValue $ path ++ "static/img/header.svg")
    div ! class_ "headercontainer" $ ul "" ! class_ "headerright"

-- Missing root-path adjustment
infBackHeader :: String -> Html
infBackHeader inf = do
  div ! class_ "header" $ do
    div ! class_ "headercontainer" $ ul ! class_ "headerleft" $ do
      li ! class_ "headerentry" $ toHtml inf
      li ! class_ "headerentry" $ ""
    div ! class_ "headercontainer" $ img ! src "static/img/header.svg"
    div ! class_ "headercontainer" $ ul ! class_ "headerright" $ do
      li ! class_ "headerentry" $ ""
      li ! class_ "headerentry" $ a "home" ! href "/" ! class_ "headerlink"

darkHeader :: String -> Html
darkHeader path = do
  div ! class_ "header" $ do
    div ! class_ "headercontainer" $ ul ! class_ "headerleft" $ do
      headerEntry "elfeck"
      headerEntry "whyiliketrees"
    div ! class_ "headercontainer" $ img
      ! src (stringValue $ path ++ "static/img/header_dark.svg")
    div ! class_ "headercontainer" $ ul ! class_ "headerright" $ do
      headerEntry "math and stuff"
      headerEntry "drivel"

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
      div "& written in Haskell" ! class_ "footerinfo"
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

{-
 Site Body Container
-}
siteBody :: Html -> Html
siteBody h = div ! class_ "sitebody" $ (div ! class_ "innerbody" $ h)

indexBody :: Html -> Html
indexBody h = div ! class_ "sitebody" $ do
  div ! class_ "innerbody indexBody" $ h
  div ! class_ "sidepanel indexSP" $ ""

genericBody :: String -> Html -> Html
genericBody "" h = siteBody h
genericBody name h = div ! class_ "sitebody" $ do
  div ! class_ (stringValue $ "innerbody " ++ name ++ "Body") $ h
  div ! class_ (stringValue $ "sidepanel " ++ name ++ "SP") $ ""

site404 :: Html
site404 = div "Sorry nothing to see here" ! class_ "testbody"

siteInvPid :: Html
siteInvPid = div ! class_ "testbody" $
             "Invalid pid. Something went horribly wrong :("

{-
 Edit Page
-}
siteEdit :: [(PostId, Post)] -> Html
siteEdit posts = do
  script "" ! src "static/js/lib/jquery-2.1.3.min.js"
  script "" ! src "static/js/button.js"
  script "" ! src "static/js/edit.js"
  div ! class_ "colbody" $ do
    div ! class_ "colleft" $ do
      input ! class_ "stdinput" ! id "eTitle"
      input ! class_ "stdinput" ! id "eCategories"
      input ! class_ "stdinput" ! id "eType"
      input ! class_ "stdinput" ! id "eAccess"
      textarea "" ! id "eArea"
    div ! class_ "colright" $ do
      input ! class_ "stdinput" ! id "ePostid" ! readonly "readonly"
      input ! class_ "stdinput" ! id "ePostdate" ! readonly "readonly"
      select ! multiple "multiple" ! class_ "stdlist" ! id "eList"
        ! autocomplete "off" $ do
          option ! id "0" ! selected "selected" $ "[new post]"
          toHtml $ map postToSelect posts
      div ! id "eResponsefield" $ ""
      input ! class_ "stdinput" ! id "eDeletefield"
      div ! id "eSubmitbutton" ! class_ "button buttonidle" $ "S"
    div "" ! id "ePreview"

postToSelect :: (PostId, Post) -> Html
postToSelect (pid, post) = case postTitle post of
  Nothing -> option ! id (keyToId pid) $
             toHtml (T.concat ["[post from ", form (postModDate post), "]"])
  Just title -> option ! id (keyToId pid) $ toHtml title
  where form date = T.pack $ formatTime defaultTimeLocale "%d. %b %R" date
        keyToId pid = stringValue (show $ fromSqlKey pid)

{-
 Evexpl Page
-}
siteEvexpl :: [(SystemVisitId, SystemVisit)] -> Html
siteEvexpl visits = do
  script "" ! src "static/js/lib/jquery-2.1.3.min.js"
  script "" ! src "static/js/lib/jquery-ui.min.js"
  script "" ! src "static/js/button.js"
  script "" ! src "static/js/evexpl.js"
  script "" ! src "static/js/evexplval.js"
  script "" ! src "static/js/evexpldata.js"
  div ! class_ "colbody" $ do
    div ! class_ "colleft" ! id "xContainer" $ do
      input ! class_ "stdinput" ! id "xRegion" ! autocomplete "off"
      div ! class_ "xSpacer" $ ""
      input ! class_ "stdinput xSite" ! id "s_0" ! autocomplete "off"
      input ! class_ "stdinput xType" ! id "t_0" ! autocomplete "off"
    div ! class_ "colright" $ do
      input ! class_ "stdinput" ! id "xEntryid" ! readonly "readonly"
      input ! class_ "stdinput" ! id "xEntrydate" ! readonly "readonly"
      select ! class_ "stdlist" ! multiple "multiple" ! id "xList"
        ! autocomplete "off" $ do
          option ! id "0" ! selected "selected" $ "[new entry]"
          toHtml $ map visitToSelect visits
      div ! id "xResponsefield" ! class_ "stdinput" $ ""
      input ! class_ "stdinput" ! id "xDeletefield"
      div ! id "xSubmitbutton" ! class_ "button buttonidle" $ "S"

visitToSelect :: (SystemVisitId, SystemVisit) -> Html
visitToSelect (eid, visit) =
  option ! id (keyToId eid) ! class_ "listEntry" $
  toHtml (T.concat ["[", systemVisitRegion visit, " at ",
                    (form $ systemVisitCrtDate visit)])
  where form date = T.pack $ formatTime defaultTimeLocale "%H:%M]" date
        keyToId eid = stringValue (show $ fromSqlKey eid)

{-
 Login Page
-}
siteLogin :: Html
siteLogin = do
  script "" ! src "static/js/lib/jquery-2.1.3.min.js"
  script "" ! src "static/js/button.js"
  script "" ! src "static/js/login.js"
  div ! id "lBody" $ do
    div "name" ! class_ "lInfo"
    input ! class_ "lInput" ! id "lUsername" ! type_ "text"
    div "" ! id "lSpacer"
    div "pass"! class_ "lInfo"
    input ! class_ "lInput" ! id "lPw" ! type_ "password"
    div ! id "lResponsecontainer" $ do
      div "login" ! id "lSubmitbutton" ! class_ "button buttonidle"
      div "" ! id "lResponsefield"

{-
 whyiliketrees
-}
-- Adjusted path to be /games/whyiliketrees in ../
whyiliketreesBody :: Html
whyiliketreesBody = do
  script "" ! src "../static/games/whyiliketrees.js"
  link ! href "../static/css/whyiliketrees.css" ! rel "stylesheet"
    ! type_ "text/css"
  div ! id "main" $ do
    div "" ! class_ "inf inf1" ! id "info1"
    div "" ! class_ "inf inf2" ! id "info2"
    div "" ! class_ "inf inf1" ! id "info3"
    div "" ! class_ "inf inf2" ! id "info4"
    div "" ! class_ "inf inf1" ! id "info5"
    div "" ! class_ "inf inf2" ! id "info6"
    canvas "" ! id "canvas" ! readonly "readonly"
    textarea "" ! id "console"
    div ! class_ "controls" ! id "info7" $ do
      "Controls: WASD, Space, Space+Shift, Arrow Keys"
      br
      "Toggle Debug: P, Toggle Mouse I"

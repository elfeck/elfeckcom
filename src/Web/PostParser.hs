{-# LANGUAGE OverloadedStrings #-}

module Web.PostParser where

import Prelude hiding (div, id)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Time
import Data.Time.Format
import Database.Persist.Sql (fromSqlKey)

import Text.Blaze.Html (toHtml, toValue, textValue, preEscapedToHtml)
import Text.Blaze.Html5 (Html, (!), div, img, ul, ol, li, a, i ,b, link, br)
import Text.Blaze.Html5.Attributes (href, rel, src, type_, class_, id)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Model.Types
import Web.BetterMdParser

renderPost :: Post -> Int -> T.Text
renderPost post renderType =
  toStrict $ renderHtml $ parsePost post renderType

renderDrivelPost :: (PostId, Post) -> UTCTime -> T.Text
renderDrivelPost (pid, post) now = toStrict $ renderHtml $ do
  putHtml (drivelTitleLine (postTitle post) (postCategories post)
           (postCrtDate post) now (T.pack $ show $ fromSqlKey pid))
  putHtml $ parseContent $ postContent post

parsePost :: Post -> Int -> Html
parsePost post 0 = putHtml $ parseContent $ postContent post -- stc site
parsePost post 1 = do
  putHtml (postTitleLine (postTitle post) (postCategories post)
           (postCrtDate post))
  putHtml $ parseContent $ postContent post
parsePost post 2 = do
  putHtml $ postTitleLine Nothing Nothing (postCrtDate post)
  putHtml $ parseContent $ postContent post

putHtml (Just h) = h
putHtml Nothing = return ()

drivelTitleLine :: Maybe T.Text -> Maybe [T.Text] -> UTCTime -> UTCTime
                -> T.Text -> Maybe Html
drivelTitleLine (Just title) (Just cats) crt _ postId = Just $ do
  div ! class_ "driveltitlebar" $ do
    div ! class_ "driveltitle" $
      (a ! href (textValue $ T.concat ["/drivel/post/", postId])
       ! class_ "plink" $ (toHtml title))
    div ! class_ "drivelcats" $ toHtml (map cToHtml cats)
    div ! class_ "drivelcrtdate" $ (toHtml $ formatTime
                                    defaultTimeLocale "%d. %b. %Y" crt)
drivelTitleLine _ _ crt now _ = Just $ do
  div ! class_ "drivelemptybar" $ (toHtml $ (timediff now crt) ++ " ago")

timediff now crt
  | raw <= 60 = (show $ conv raw) ++ " sec" ++ (plr $ conv raw)
  | raw > 60*60*24*7 = (show $ conv raw) ++ " week" ++ (plr $ conv raw)
  | raw > 60*60*24 = (show $ conv raw) ++ " day" ++ (plr $ conv raw)
  | raw > 60*60 = (show $ conv raw) ++ " hour" ++ (plr $ conv raw)
  | raw > 60 = (show $ conv raw) ++ " min" ++ (plr $ conv raw)
  where raw = diffUTCTime now crt
        conv raw | raw <= 60 = floor raw
                 | raw > 60*60*24*7 = floor (raw / (60*60*24*7))
                 | raw > 60*60*24 = floor (raw / (60*60*24))
                 | raw > 60*60 = floor (raw / (60*60))
                 | raw > 60 = floor (raw / 60)
        plr r | r == 1 = ""
              | otherwise = "s"

postTitleLine :: Maybe T.Text -> Maybe [T.Text] -> UTCTime -> Maybe Html
postTitleLine (Just title) (Just cats) crt = Just $ do
  div ! class_ "posttitlebar" $ do
    div ! class_ "posttitle" $ toHtml title
    div ! class_ "postcats" $ toHtml (map cToHtml cats)
    div ! class_ "postcrtdate" $ (toHtml $ formatTime
                                  defaultTimeLocale "%d. %b. %Y" crt)
postTitleLine _ _ crt = Just $ do
  div ! class_ "posttitlebar" $ do
    "Drivel entry from "
    div ! class_ "postdrivelcrtdate" $ (toHtml $ formatTime
                                        defaultTimeLocale "%d. %b. %Y" crt)
    ":"


cToHtml cat = div ! class_ "postcat" $ toHtml (T.concat [cat, " "])

parseContent :: T.Text -> Maybe Html
parseContent cont = fmap toHtml $ (docToHtml $ parseMd cont)

docToHtml :: Doc -> Maybe [Html]
docToHtml [] = Nothing
docToHtml doc = Just $ map secToHtml doc

secToHtml :: Sec -> Html
secToHtml (Par bs) = div ! class_ "par" $ toHtml $ map blockToHtml bs
secToHtml (Hdl (n, es)) =
  div ! class_ (textValue (T.append "hdl" $ T.pack $ (show n))) $
  toHtml $ map eleToHtml es
secToHtml (Htm t) = preEscapedToHtml t

blockToHtml :: Block -> Html
blockToHtml (List els) = ul ! class_ "ul" $ toHtml $ map toListEle els
blockToHtml (Enum (n, els)) = ol ! class_ "ol" $ toHtml $ map toListEle els
blockToHtml (Norm els) = toHtml $ map eleToHtml els

toListEle :: [Ele] -> Html
toListEle els = li ! class_ "li" $ toHtml $ map eleToHtml els

eleToHtml :: Ele -> Html
eleToHtml Newline = br
eleToHtml (Plain t) = toHtml t
eleToHtml (Italic t) = i $ toHtml t
eleToHtml (Bold t) = b $ toHtml t
eleToHtml (Link (d, l)) = a ! (href $ toValue l) ! class_ "link" $ toHtml d
eleToHtml (Image (d, l)) = img ! (src $ toValue l) ! class_ "imag"

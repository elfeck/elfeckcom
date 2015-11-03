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

renderPost :: (PostId, Post) -> Int -> T.Text
renderPost (pid, post) renderType =
  toStrict $ renderHtml $ parsePost (pid, post) renderType

parsePost :: (PostId, Post) -> Int -> Html
parsePost (_, post) 0 = putHtml $ parseContent $ postContent post -- static site
parsePost (_, post) 1 = putHtml $ parseContent $ postContent post -- blog posts
parsePost (pid, post) 2 = do
  putHtml (drivelTitleLine (postTitle post) (postCategories post)
           (postCrtDate post) (T.pack $ show $ fromSqlKey pid))
  putHtml $ drivelContent $ postContent post
parsePost post _ = ""

putHtml (Just h) = h
putHtml Nothing = return ()

drivelTitleLine :: Maybe T.Text -> Maybe [T.Text] -> UTCTime -> T.Text
                   -> Maybe Html
drivelTitleLine (Just title) (Just cats) crt postId = Just $ do
  div ! class_ "posttitlebar" $ do
    div ! class_ "posttitle" $
      (a ! href (textValue $ T.concat ["/drivel/post/", postId])
       ! class_ "plink" $ (toHtml title))
    div ! class_ "postcats" $ toHtml (map cToHtml cats)
    div ! class_ "postcrtdate" $ (toHtml $ formatTime
                                  defaultTimeLocale "%F" crt)
      where cToHtml cat = div ! class_ "postcat" $ toHtml (T.concat [cat, " "])
drivelTitleLine _ _ _ _ = undefined

drivelContent :: T.Text -> Maybe Html
drivelContent cont = parseContent cont

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

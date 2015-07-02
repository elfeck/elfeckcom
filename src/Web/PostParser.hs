{-# LANGUAGE OverloadedStrings #-}

module Web.PostParser where

import Prelude hiding (div, id)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)

import Text.Blaze.Html (toHtml, toValue, textValue, preEscapedToHtml)
import Text.Blaze.Html5 (Html, (!), div, img, ul, ol, li, a, i ,b, link, br)
import Text.Blaze.Html5.Attributes (href, rel, src, type_, class_, id)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Model.Types
import Web.BetterMdParser

renderPost :: Post -> T.Text
renderPost post = toStrict $ renderHtml $ parsePost post

parsePost :: Post -> Html
parsePost post = case postPtype post of
  0 -> do
    putHtml $ parseContent $ postContent post
  _ -> do
    putHtml $ parseTitle $ postTitle post
    putHtml $ parseCategories $ postCategories post
    putHtml $ parseContent $ postContent post
  where putHtml (Just h) = h
        putHtml Nothing = return ()

parseTitle :: Maybe T.Text -> Maybe Html
parseTitle Nothing = Nothing
parseTitle (Just title) = Just $ div ! class_ "title" $ (toHtml title)

parseCategories :: Maybe [T.Text] -> Maybe Html
parseCategories Nothing = Nothing
parseCategories (Just cat) =
  Just $ div ! class_ "categories" $ (toHtml $ T.intercalate ", " cat)

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

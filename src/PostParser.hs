{-# LANGUAGE OverloadedStrings #-}

module PostParser where

import Prelude hiding (div, id)
import Data.Maybe
import qualified Data.Text.Lazy as L

import Text.Blaze.Html (toHtml, toValue)
import Text.Blaze.Html5 (Html, (!), div, img, ul, li, a, i ,b, link)
import Text.Blaze.Html5.Attributes (href, rel, src, type_, class_, id)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Model
import MdParser

parsePreview :: [L.Text] -> L.Text
parsePreview (typ : pid : content) =
  case typ of
   "0" -> renderHtml $ do
     link ! href "/css/site.css" ! rel "stylesheet" ! type_ "text/css"
     putHtml $ parseContent (content !! 2) -- content
   _ -> renderHtml $ do
     putHtml $ parseTitle (content !! 0)
     putHtml $ parseCategories (content !! 1)
     putHtml $ parseContent (content !! 2)
  where putHtml (Just h) = h
        putHtml Nothing = return ()

parsePost :: (PostId, Post) -> L.Text
parsePost = undefined

parseTitle :: L.Text -> Maybe Html
parseTitle "" = Nothing
parseTitle title = Just $ div ! class_ "title" $ (toHtml title)

parseCategories :: L.Text -> Maybe Html
parseCategories "" = Nothing
parseCategories cat = Just $ div ! class_ "categories" $ (toHtml cat)

parseContent :: L.Text -> Maybe Html
parseContent cont = fmap toHtml $ (docToHtml $ parseMd cont)

docToHtml :: Doc -> Maybe [Html]
docToHtml [] = Nothing
docToHtml doc = Just $ map blockToHtml doc

blockToHtml :: Block -> Html
blockToHtml (Par es) = div ! class_ "par" $ toHtml $ map eleToHtml es
blockToHtml (Hdl es) = div ! class_ "hdl" $ toHtml $ map eleToHtml es

eleToHtml :: Ele -> Html
eleToHtml (Plain t) = toHtml t
eleToHtml (Italic t) = i $ toHtml t
eleToHtml (Bold t) = b $ toHtml t
eleToHtml (Link (c, h)) = a ! class_ "link" ! href (toValue h) $ toHtml c
eleToHtml (Image (c, h)) = img ! class_ "imag" ! src (toValue h)

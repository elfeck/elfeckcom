{-# LANGUAGE OverloadedStrings #-}

module EntryParser where

import Prelude hiding (div, id)
import Data.Maybe
import qualified Data.Text.Lazy as L

import Text.Blaze.Html (toHtml)
import Text.Blaze.Html5 (Html, (!), div, img, ul, li, a)
import Text.Blaze.Html5.Attributes (href, rel, src, type_, class_, id)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import MdParser

parseTitle :: L.Text -> Maybe Html
parseTitle "" = Nothing
parseTitle title = Just $ div (toHtml title)

parseCategories :: L.Text -> Maybe Html
parseCategories "" = Nothing
parseCategories cat = Just $ div (toHtml cat)

parseContent :: L.Text -> Maybe Html
parseContent cont = case docToHtml $ parseToDoc cont of
  Nothing -> Nothing
  Just xs -> Just (toHtml $ xs)

parseEdit :: [L.Text] -> L.Text
parseEdit content = renderHtml $ do
  putHtml $ parseTitle (content !! 0)
  putHtml $ parseCategories (content !! 1)
  putHtml $ parseContent (content !! 2)
  where putHtml (Just h) = h
        putHtml Nothing = return ()

docToHtml :: Docum -> Maybe [Html]
docToHtml [] = Nothing
docToHtml doc = Just $ map secToHtml doc

secToHtml :: Section -> Html
secToHtml (Headl t) = div (toHtml t) ! class_ "headline"
secToHtml (Parag t) = div (toHtml t) ! class_ "parag"

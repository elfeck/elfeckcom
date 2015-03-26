{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.List
import Data.Maybe
import Text.XML.Light.Input
import Text.XML.Light.Types
import Text.XML.Light.Proc


parseSvg = do
  svg <- readFile "./static/header.svg"
  let k = goThrough (Elem (fromJust $ parseXMLDoc svg)) []
  print (fromJust $ parseXMLDoc svg)


goThrough :: Content -> [Bool] -> [Bool]
goThrough (Elem (Element _ attr childs _)) l
  | length childs == 0 = [True]
  | otherwise =
      l ++ [attrHasFill attr] ++
      (foldl (++) [] [goThrough ch [] | ch <- childs])

eleIsG (Element (QName n _ _) _ _ _) = n == "g"

pathHasFill :: Element -> Bool
pathHasFill (Element (QName n _ _) attr _ _) =
  n == "path" && attrHasFill attr

attrHasFill :: [Attr] -> Bool
attrHasFill attr = not (null [a | a <- attr, isFilled a])

isFilled (Attr (QName n _ _) v) =
  n == "style" && isInfixOf "fill:#" v

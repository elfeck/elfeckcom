{-# LANGUAGE OverloadedStrings #-}

module EditParser where

import Prelude hiding (div, id)
import Data.Maybe
import qualified Data.Text.Lazy as L

import Text.Blaze.Html (toHtml)
import Text.Blaze.Html5 (Html, (!), div, img, ul, li, a)
import Text.Blaze.Html5.Attributes (href, rel, src, type_, class_, id)
import Text.Blaze.Html.Renderer.Text (renderHtml)


-- Line-wise intermediate
type Docum = [Section]
data Section = Headl L.Text
             | Parag L.Text
             deriving Show

-- The real stuff
type Doc = [Block]
data Block = Par [Ele]
           | Hdl [Ele]
           deriving Show
data Ele = Plain L.Text
         | Italic L.Text
         deriving Show

main = do
  f <- readFile "test.txt"
  --print $ parseToDoc $ L.pack f
  print $ docToReal $ parseToDoc $ L.pack f


{-
 Docum to the real stuff
-}
--bla :: Section -> Block
docToReal :: Docum -> Doc
docToReal doc = map bla doc

bla (Headl t) = Hdl $ kla t False []
bla (Parag t) = Par $ kla t False []

kla :: L.Text -> Bool -> [Ele] -> [Ele]
kla "" _ acc = reverse acc
kla t False []
  | c == "*" && lookahead (L.tail t) = kla (L.tail t) True [Italic ""]
  | otherwise = kla (L.tail t) False [Plain c]
  where c = L.singleton $ L.head t
kla t False (h : acc)
  | c == "*" && lookahead (L.tail t) = kla (L.tail t) True
                                       ((Italic "") : h : acc)
  | otherwise = kla (L.tail t) False ((concatEle h c) : acc)
  where c =  L.singleton $ L.head t
kla t True (h : acc)
  | c == "*" = kla (L.tail t) False ((Plain "") : h : acc)
  | otherwise = kla (L.tail t) True ((concatEle h c) : acc)
  where c = L.singleton $ L.head t

lookahead = L.isInfixOf "*"
concatEle (Plain t) tn = Plain $ L.append t tn
concatEle (Italic t) tn = Italic $ L.append t tn


{-
 Text to Doc
-}
parseToDoc :: L.Text -> Docum
parseToDoc cont = sanEmpty $ go (reverse $ L.lines cont) []
  where go [] doc = doc
        go (l : ls) doc
          | isHeadS l = go ls $ (Headl "") : doc
          | isParaS l = go ls $ (Parag "") : doc
          | null doc = go ls $ (Parag l) : doc
          | isAboveHeadS doc = go ls $ (Parag l) : doc
          | otherwise = go ls ((appendToS (head doc) l) : (tail doc))
        sanEmpty doc = [s | s <- doc, (contentS s) /= ""]


appendToS (Headl t) tn = Headl $ L.append tn (apN t)
appendToS (Parag t) tn = Parag $ L.append tn (apN t)
apN t = if t == "" then t else L.append "\n" t

contentS (Headl t) = t
contentS (Parag t) = t

isHeadS line | L.length line < 2 = False
             | otherwise = L.null (L.filter (/= '=') line)

isParaS line = line == ""

isAboveHeadS ((Headl t) : doc) = (length $ L.lines t) == 1
isAboveHeadS _ = False

{-
 TODO
-}
parseTitle :: L.Text -> Maybe Html
parseTitle "" = Nothing
parseTitle title = Just $ div (toHtml title)

parseCategories :: L.Text -> Maybe Html
parseCategories "" = Nothing
parseCategories cat = Just $ div (toHtml cat)

{-
 HTML Stuff
-}
parseEdit :: [L.Text] -> L.Text
parseEdit content = renderHtml $ do
  putHtml $ parseTitle (content !! 0)
  putHtml $ parseCategories (content !! 1)
  putHtml $ parseContent (content !! 2)
  where putHtml (Just h) = h
        putHtml Nothing = return ()

parseContent :: L.Text -> Maybe Html
parseContent cont = case docToHtml $ parseToDoc cont of
  Nothing -> Nothing
  Just xs -> Just (toHtml $ xs)

docToHtml :: Docum -> Maybe [Html]
docToHtml [] = Nothing
docToHtml doc = Just $ map secToHtml doc

secToHtml :: Section -> Html
secToHtml (Headl t) = div (toHtml t) ! class_ "headline"
secToHtml (Parag t) = div (toHtml t) ! class_ "parag"

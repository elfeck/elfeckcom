{-# LANGUAGE OverloadedStrings #-}

module MdParser where

import qualified Data.Text.Lazy as L


type IDoc = [Section]
data Section = IHdl L.Text | IPar L.Text deriving Show

type Doc = [Block]
data Block = Par [Ele] | Hdl [Ele] deriving Show
data Ele = Plain L.Text
         | Italic L.Text
         | Bold L.Text
         | Link (L.Text, L.Text)
         | Image (L.Text, L.Text)
         deriving Show

data PState = SItalic
            | SBold
            | SPlain
            | SDefault
            | SLinkT | SLinkH
            | SImagT | SImagH
            deriving (Eq, Show)

main = do
  f <- readFile "test.txt"
  --print $ parseToDoc $ L.pack f
  print $ parseMd $ L.pack f


parseMd :: L.Text -> Doc
parseMd = intermToDoc . parseToInterm

intermToDoc :: IDoc -> Doc
intermToDoc doc = map parseSection doc

parseSection (IHdl t) = Hdl $ prs t SDefault []
parseSection (IPar t) = Par $ prs t SDefault []

prs :: L.Text -> PState -> [Ele] -> [Ele]
prs "" _ acc = reverse acc
prs t SDefault doc
  | tryFor SImagT t = prs (trim SImagT t) SImagT ((Image ("", "")) : doc)
  | tryFor SLinkT t = prs (trim SLinkT t) SLinkT ((Link ("", "")) : doc)
  | tryFor SBold t = prs (trim SBold t) SBold ((Bold "") : doc)
  | tryFor SItalic t = prs (trim SItalic t) SItalic ((Italic "") : doc)
  | otherwise = prs (trim SPlain t) SPlain ((Plain c) : doc)
  where c = L.singleton $ L.head t
prs t SPlain doc
  | tryFor SImagT t = prs (trim SImagT t) SImagT ((Image ("", "")) : doc)
  | tryFor SLinkT t = prs (trim SLinkT t) SLinkT ((Link ("", "")) : doc)
  | tryFor SBold t = prs (trim SBold t) SBold ((Bold "") : doc)
  | tryFor SItalic t = prs (trim SItalic t) SItalic ((Italic "") : doc)
  | otherwise = prs (trim SPlain t) SPlain (concatToDoc doc c SPlain)
  where c = L.singleton $ L.head t
prs t state doc
  | checkFor SImagT t && state == SImagT = prs (L.drop 2 t) SImagH doc
  | checkFor SLinkT t && state == SLinkT = prs (L.drop 2 t) SLinkH doc
  | checkFor state t = prs (trim state t) SDefault doc
  | otherwise = prs (trim SPlain t) state (concatToDoc doc c state)
  where c = L.singleton $ L.head t

trim :: PState -> L.Text -> L.Text
trim SBold t = L.drop 2 t
trim SImagT t = L.drop 2 t
trim _ t = L.drop 1 t

tryFor :: PState -> L.Text -> Bool
tryFor SLinkT t = L.take 1 t == "[" &&
                  L.isPrefixOf "[]()" (L.filter isLinkChar t)
                  -- TODO Fix []asdasd()
tryFor SImagT t = L.take 1 t == "!" &&
                  L.isPrefixOf "![]()" (L.filter isImagChar t)
                  -- TODO Fix !asdasd[]asdasd()
tryFor state t = checkFor state t && lookAhead state t

isLinkChar c = c == '[' || c == ']' || c == '(' || c == ')'
isImagChar c = isLinkChar c || c == '!'

checkFor :: PState -> L.Text -> Bool
checkFor SItalic t = L.take 1 t == "*"
checkFor SBold t = L.take 2 t == "**"
checkFor SLinkT t = L.take 1 t == "]"
checkFor SLinkH t = L.take 1 t == ")"
checkFor SImagT t = L.take 1 t == "]"
checkFor SImagH t = L.take 1 t == ")"
checkFor _ _ = True

lookAhead :: PState -> L.Text -> Bool
lookAhead SItalic t = L.length t > 1 && L.isInfixOf "*" (L.tail t)
lookAhead SBold t = L.length t > 2 &&  L.isInfixOf "**" (L.drop 2 t)
lookAhead _ _ = True

concatEle (Plain t) tn = Plain $ L.append t tn
concatEle (Italic t) tn = Italic $ L.append t tn
concatEle (Bold t) tn = Bold $ L.append t tn

concatToDoc ((Link (t,h)) : doc) tn SLinkT = (Link (L.append t tn, h)) : doc
concatToDoc ((Link (t,h)) : doc) tn SLinkH = (Link (t, L.append h tn)) : doc
concatToDoc ((Image (t,h)) : doc) tn SImagT = (Image (L.append t tn, h)) : doc
concatToDoc ((Image (t,h)) : doc) tn SImagH = (Image (t, L.append h tn)) : doc
concatToDoc (d : doc) tn _ = (concatEle d tn) : doc


{-
 Text to Doc
-}
parseToInterm :: L.Text -> IDoc
parseToInterm cont = sanEmpty $ go (reverse $ L.lines cont) []
  where go [] doc = doc
        go (l : ls) doc
          | isHeadS l = go ls $ (IHdl "") : doc
          | isParaS l = go ls $ (IPar "") : doc
          | null doc = go ls $ (IPar l) : doc
          | isAboveHeadS doc = go ls $ (IPar l) : doc
          | otherwise = go ls ((appendToS (head doc) l) : (tail doc))
        sanEmpty doc = [s | s <- doc, (contentS s) /= ""]


appendToS (IHdl t) tn = IHdl $ L.append tn (apN t)
appendToS (IPar t) tn = IPar $ L.append tn (apN t)
apN t = if t == "" then t else L.append "\n" t

contentS (IHdl t) = t
contentS (IPar t) = t

isHeadS line | L.length line < 2 = False
             | otherwise = L.null (L.filter (/= '=') line)

isParaS line = line == ""

isAboveHeadS ((IHdl t) : doc) = (length $ L.lines t) == 1
isAboveHeadS _ = False

{-# LANGUAGE OverloadedStrings #-}

module BetterMdParser where

import Data.Char
import Data.List
import qualified Data.Text as T

data Level1 = Empty | Equals T.Text | HtmlTagO T.Text | HtmlTagC T.Text |
              Raw Line | Headl (Int, Line)
            deriving Show

data Level2 = BlockNorm [Level1] | BlockHtml [Level1]
            deriving Show
data L2State = L2N | L2O (Int, T.Text) | L2C

data Level3 = Headline (Int, Line) | HtmlBlock [Line] | Paragraph [Line]
            deriving Show
data Line = RawLine T.Text | ListLine T.Text | EnumLine (Int, T.Text)
            deriving Show

data Level4 = Hdl (Int, [Ele]) | Htm T.Text | Par [Block] deriving Show

data Block = List [[Ele]] | Enum (Int, [[Ele]]) | Norm [Ele] deriving Show
data Ele = Newline | Plain T.Text | Italic T.Text | Bold T.Text |
           Link (T.Text, T.Text) | Image (T.Text, T.Text) |
           RawEle T.Text deriving Show


main = do
  f <- readFile "src/test.txt"
  let pl1 = procL1 $ T.pack f
  let pl2 = procL2 pl1 L2C []
  let pl3 = procL3 pl2 []
  let pl3' = procL3Lines pl3 []
  let pl4 = procL4 pl3' []
  print $ pl4


procL1 :: T.Text -> [Level1]
procL1 text = prs (T.lines text) $ []
  where prs [] doc = reverse doc
        prs ("" : lin) doc = prs lin (Empty : doc)
        prs (l : lin) doc | eqs l /= "" = prs lin ((Equals $ eqs l) : doc)
                          | isHtmlTagC l = prs lin ((HtmlTagC l) : doc)
                          | isHtmlTagO l = prs lin ((HtmlTagO l) : doc)
                          | isHl $ hl l = prs lin ((Headl $ hl l) : doc)
                          | otherwise = prs lin ((Raw $ RawLine l) : doc)
          where eqs l | T.length l >= 2 && (T.filter (/= '=') l) == "" = l
                      | otherwise = ""
                isHtmlTagO l = T.head l == '<' && T.last l == '>'
                isHtmlTagC l = T.length l >= 2 && T.take 2 l == "</" &&
                               T.last l == '>'
                hl l = let lspan = T.span (== '#') l
                       in (T.length $ fst lspan, RawLine $ snd lspan)
                isHl (c, RawLine t) = c > 0 && T.length t > 0 &&
                                      T.head t == ' '

procL2 :: [Level1] -> L2State -> [Level2] -> [Level2]
procL2 [] _ doc = reverse doc
-- closing state: new block
procL2 (l : ls) L2C doc = case tryHtml (l : ls) of
  Nothing -> procL2 ls L2N ((BlockNorm [l]) : doc)
  Just tag -> procL2 ls (L2O (0, tag)) ((BlockHtml [l]) : doc)
-- open state: look for closing
procL2 (l : ls) (L2O (n, tag)) doc
  | isClosing l tag = case n of
                       0 -> procL2 ls L2C $ appendTH l doc
                       m -> procL2 ls (L2O (m - 1, tag)) $ appendTH l doc
  | isOpening l tag = procL2 ls (L2O (n + 1, tag)) $ appendTH l doc
  | otherwise = procL2 ls (L2O (n, tag)) $ appendTH l doc
-- normal state: look for open
procL2 (l : ls) L2N doc = case tryHtml (l : ls) of
  Nothing -> procL2 ls L2N $ appendTH l doc
  Just tag -> procL2 ls (L2O (0, tag)) ((BlockHtml [l]) : doc)

appendTH line ((BlockNorm l1) : doc) = (BlockNorm (l1 ++ [line])) : doc
appendTH line ((BlockHtml l1) : doc) = (BlockHtml (l1 ++ [line])) : doc

tryHtml :: [Level1] -> Maybe T.Text
tryHtml (l1 : l1s) = case findOpening l1 of
  Nothing -> Nothing
  Just tag -> if length [lin | lin <- l1s, isOpening lin tag] <=
                 (length [lin | lin <- l1s, isClosing lin tag]) - 1
              then Just tag
              else Nothing

findOpening (HtmlTagO t) = Just $ extractTag (HtmlTagO t)
findOpening _ = Nothing

isClosing (HtmlTagC t) tag = tag == extractTag (HtmlTagC t)
isClosing _ _ = False

isOpening (HtmlTagO t) tag = tag == extractTag (HtmlTagO t)
isOpening _ _ = False

extractTag (HtmlTagO t) = fst $ T.span (\c -> c /= ' ' && c /= '>') (T.tail t)
extractTag (HtmlTagC t) = (T.drop 2 (T.init t))

procL3 :: [Level2] -> [Level3] -> [Level3]
procL3 [] doc = reverse doc
procL3 ((BlockHtml l1s) : l2s) doc =
  procL3 l2s $ (HtmlBlock $ map onlyContent l1s) : doc
procL3 ((BlockNorm l1s) : l2s) doc = procL3 l2s $ pL3B l1s doc

pL3B [] doc = doc
pL3B ((Headl (n, l)) : l1s) doc = pL3B l1s ((Headline (n, l)) : doc)
pL3B (Empty : l1s) doc = pL3B l1s ((Paragraph []) : doc)
pL3B (l1 : l1s) doc
  | (not $ null l1s) && (isEquals $ head l1s) =
      pL3B (tail l1s) ((Headline (1, onlyContent l1)) : doc)
  | isPara doc = pL3B l1s $ appTH l1 doc
  | otherwise = pL3B l1s ((Paragraph [onlyContent l1]) : doc)

isEquals (Equals _) = True
isEquals _ = False

isPara [] = False
isPara ((Paragraph ls) : doc) = True
isPara _ = False

appTH l ((Paragraph ls) : doc) = (Paragraph $ ls ++ [onlyContent l]) : doc

onlyContent (Raw l) = l
onlyContent (Headl (n, l)) = l
onlyContent (HtmlTagO t) = RawLine t
onlyContent (HtmlTagC t) = RawLine t
onlyContent (Equals t) = RawLine t
onlyContent _ = RawLine ""

procL3Lines :: [Level3] -> [Level3] -> [Level3]
procL3Lines [] doc = reverse doc
procL3Lines ((Headline h) : l3s) doc = procL3Lines l3s ((Headline h) : doc)
procL3Lines ((HtmlBlock l) : l3s) doc = procL3Lines l3s ((HtmlBlock l) : doc)
procL3Lines ((Paragraph l) : l3s) doc =
  procL3Lines l3s ((Paragraph (prL3P l [])) : doc)

prL3P :: [Line] -> [Line] -> [Line]
prL3P [] acc = reverse acc
prL3P ((RawLine t) : ls) acc
  | isListItem t = prL3P ls ((ListLine $ T.drop 2 t) : acc)
  | isEnumItem t =
      prL3P ls ((EnumLine $ (digitToInt $ T.head t, T.drop 3 t)) : acc)
  | (not $ null acc) = prL3P ls ((RawLine t) : acc)
  | otherwise = prL3P ls ((RawLine t) : acc)

isRaw (RawLine _) = True
isRaw _ = False

isListItem t = T.length t >= 2 && (T.take 2 t) == "* "
isEnumItem t = T.length t >= 2 && isDigit (T.head t) && (T.index t 1) == '.'

procL4 :: [Level3] -> [Level4] -> [Level4]
procL4 [] doc = procL4Col (reverse doc) []
procL4 ((Headline (n, t)) : l3s) doc =
  procL4 l3s ((Hdl (n, [RawEle $ textFromLine t])) : doc)
procL4 ((HtmlBlock ls) : l3s) doc =
  procL4 l3s ((Htm $ T.unlines $ map textFromLine ls) : doc)
procL4 ((Paragraph ls) : l3s) doc =
  procL4 l3s ((Par $ groupAndMerge ls) : doc)

procL4Col [] doc = reverse doc
procL4Col ((Par block) : d) doc =
  procL4Col d $ ((Par $ collapseNormRaw block []) : doc)
procL4Col (e : d) doc = procL4Col d (e : doc)

groupAndMerge ls = map blockify $ groupBy sameLineType ls

blockify :: [Line] -> Block
blockify ls
  | arePlainLines ls = Norm $ map toRaw ls
  | areListLines ls = List $ map (\e -> [e]) $ procListLines ls []
  | areEnumLines ls =
      Enum $ (minNum ls 10, map (\e -> [e]) $ procEnumLines ls [])

procListLines [] doc = reverse doc
procListLines ((ListLine t) : ls) doc = procListLines ls ((RawEle t) : doc)
procListLines ((RawLine t) : ls) ((RawEle to) : doc) =
  procListLines ls ((RawEle $ T.unlines [to, t]) : doc)

procEnumLines [] doc = reverse doc
procEnumLines ((EnumLine (_, t) : ls)) doc =
  procEnumLines ls ((RawEle t) : doc)
procEnumLines ((RawLine t) : ls) ((RawEle to) : doc) =
  procEnumLines ls ((RawEle $ T.unlines [to, t]) : doc)

collapseNormRaw :: [Block] -> [Block] -> [Block]
collapseNormRaw [] block = reverse block
collapseNormRaw ((Norm eles) : bl) block
  = collapseNormRaw bl ((Norm [RawEle (T.unlines $ map toText eles)]) : block)
collapseNormRaw (b : bl) block = collapseNormRaw bl (b : block)

toText (RawEle t) = t

toRaw (RawLine t) = RawEle t
toRaw (ListLine t) = RawEle t
toRaw (EnumLine (_, t)) = RawEle t

arePlainLines [] = True
arePlainLines ((RawLine _) : _) = True
arePlainLines _ = False

areListLines [] = False
areListLines ((ListLine _) : _) = True
areListLines _ = False

areEnumLines [] = False
areEnumLines ((EnumLine _) : _) = True
areEnumLines _ = False

minNum [] n = n
minNum (EnumLine (k, t) : ls) n | k < n = minNum ls k
                                | otherwise = minNum ls n
minNum (_ : ls) n = minNum ls n

textFromLine (RawLine t) = t
textFromLine (ListLine t) = t
textFromLine (EnumLine (n, t)) = t

sameLineType (RawLine _) (RawLine _) = True
sameLineType (ListLine _) (ListLine _) = True
sameLineType (ListLine lt) (RawLine _) =
  T.length lt < 3 || (T.length lt >= 3 && (T.take 2 $ T.reverse lt) /= "  ")
sameLineType (EnumLine _) (EnumLine _) = True
sameLineType (EnumLine (_, le)) (RawLine _) =
  T.length le < 3 || (T.length le >= 3 && (T.take 2 $ T.reverse le) /= "  ")
sameLineType _ _ = False

{-# LANGUAGE OverloadedStrings #-}

module BetterMdParser where

import qualified Data.Text as T

data Level1 = Empty | Equals T.Text | HtmlTagO T.Text | HtmlTagC T.Text |
              Raw Line | Headl (Int, Line)
            deriving Show

data Level2 = BlockNorm [Level1] | BlockHtml [Level1]
            deriving Show
data L2State = L2N | L2O (Int, T.Text) | L2C

data Level3 = Headline (Int, Line) | HtmlBlock [Line] | Paragraph [Line]
            deriving Show



data Line = LineRaw T.Text
            deriving Show

main = do
  f <- readFile "src/test.txt"
  let pl1 = procL1 $ T.pack f
  let pl2 = procL2 pl1 L2C []
  let pl3 = procL3 pl2 []
  print $ pl1
  print $ "====================="
  print $ pl2
  print $ "====================="
  print $ pl3


procL1 :: T.Text -> [Level1]
procL1 text = prs (T.lines text) $ []
  where prs [] doc = reverse doc
        prs ("" : lin) doc = prs lin (Empty : doc)
        prs (l : lin) doc | eqs l /= "" = prs lin ((Equals $ eqs l) : doc)
                          | isHtmlTagC l = prs lin ((HtmlTagC l) : doc)
                          | isHtmlTagO l = prs lin ((HtmlTagO l) : doc)
                          | isHl $ hl l = prs lin ((Headl $ hl l) : doc)
                          | otherwise = prs lin ((Raw $ LineRaw l) : doc)
          where eqs l | T.length l >= 2 && (T.filter (/= '=') l) == "" = l
                      | otherwise = ""
                isHtmlTagO l = T.head l == '<' && T.last l == '>'
                isHtmlTagC l = T.length l >= 2 && T.take 2 l == "</" &&
                               T.last l == '>'
                hl l = let lspan = T.span (== '#') l
                       in (T.length $ fst lspan, LineRaw $ snd lspan)
                isHl (c, LineRaw t) = c > 0 && T.length t > 0 &&
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
onlyContent (HtmlTagO t) = LineRaw t
onlyContent (HtmlTagC t) = LineRaw t
onlyContent (Equals t) = LineRaw t
onlyContent _ = LineRaw ""

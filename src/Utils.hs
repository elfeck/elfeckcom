{-# LANGUAGE OverloadedStrings #-}

module Utils where

import qualified Data.Text as T
import Data.Text.Read
import Data.Maybe


data Route = DB T.Text Int | Redirect T.Text T.Text deriving Show
data SiteConfig = SiteConfig { database :: T.Text
                             , routes :: [Route] }
              deriving Show

--main = do
--  file <- readFile "config.txt"
--  print $ parseConfig $ T.pack file

textToInt :: T.Text -> Maybe Int
textToInt text = case decimal text of
                Left _ -> Nothing
                Right (val, "") -> Just val
                Right (val, _) -> Nothing

--parseConfig :: T.Text -> Config
parseConfig t = SiteConfig db routes
  where blocks = map T.lines $ T.splitOn "\n\n" t
        db = foldl T.append "" $ map parseDatabase blocks
        rawRoutes = foldl (++) [] $ map parseRoutes blocks
        routes = map (constructRoute . tuplify) rawRoutes

parseDatabase ("[Database]" : ts) = head $ ts
parseDatabase _ = ""

parseRoutes ("[Routes]" : ts) = map T.words ts
parseRoutes _ = []

constructRoute :: (T.Text, T.Text, T.Text) -> Route
constructRoute (url, "redirect", link) = Redirect url link
constructRoute (url, "db", pid) = DB url $ fromJust $ textToInt pid

tuplify (a : b : c : []) = (a, b, c)
tuplify _ = undefined

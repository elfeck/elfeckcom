{-# LANGUAGE OverloadedStrings #-}

module Web.JsonParser where

import qualified Data.Text as T
import Data.Time
import Data.Maybe

import Web.Utils
import Model.Types

checkJson :: [Maybe T.Text] -> Maybe [T.Text]
checkJson xs | null $ filter isNothing xs = Just (map fromJust xs)
             | otherwise = Nothing
jsonToPost :: [(T.Text, T.Text)] -> Maybe Post
jsonToPost params = case chkp of
  Nothing -> Nothing
  Just pars -> createPost pars dummyTime dummyTime
  where chkp = checkJson $ findParams params ["title", "categories",
                                              "content", "type", "access"]

-- Handles possible user error here. Should be somewhere else
-- DummyAuthor for not having to login while testing
jsonToSystemVisit :: Maybe User -> [(T.Text, T.Text)] -> Maybe SystemVisit
jsonToSystemVisit mauthor params = case (mauthor, chkp) of
  (Just author, Just pars) ->
    createSystemVisit (userName author) pars dummyTime
  (Nothing, Just pars) -> createSystemVisit (dummyAuthor) pars dummyTime
  _ -> Nothing
  where chkp = checkJson $ findParams params ["name", "region", "sites",
                                              "types"]
        dummyAuthor = "seb"

createPost :: [T.Text] -> UTCTime -> UTCTime -> Maybe Post
createPost postParam crtTime modTime
  | isNothing ty || isNothing ac = Nothing
  | otherwise = Just $ Post (procTitle $ postParam !! 0)
                (procCategories $ postParam !! 1)
                (postParam !! 2)
                crtTime
                modTime
                (fromJust ty)
                (fromJust ac)
  where ty = (textToInt $ postParam !! 3)
        ac = (textToInt $ postParam !! 4)
        procTitle "" = Nothing
        procTitle text = Just text
        procCategories "" = Nothing
        procCategories text = Just $ T.splitOn ", " text

createSystemVisit :: T.Text -> [T.Text] -> UTCTime -> Maybe SystemVisit
createSystemVisit author visitParam crtTime
  | isNothing reg || length sites /= length types = Nothing
  | otherwise = Just $ SystemVisit name (fromJust reg) (zip sites types)
                crtTime author
  where name = procName (visitParam !! 0)
        reg = procRegion (visitParam !! 1)
        sites = T.splitOn "," (visitParam !! 2)
        types = T.splitOn "," (visitParam !! 3)
        procRegion "" = Nothing
        procRegion text = Just text
        procName "" = Nothing
        procName text = Just text

dummyTime :: UTCTime
dummyTime = parseTimeOrError True defaultTimeLocale "%d.%m.%Y %H:%M"
            "01.01.2000 00:00"

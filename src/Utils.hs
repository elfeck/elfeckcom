{-# LANGUAGE OverloadedStrings #-}

module Utils where

import qualified Data.Text as T
import Data.Text.Read

textToInt :: T.Text -> Maybe Int
textToInt text = case decimal text of
                Left _ -> Nothing
                Right (val, "") -> Just val
                Right (val, _) -> Nothing

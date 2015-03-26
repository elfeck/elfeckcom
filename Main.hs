{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty (scotty, middleware)
import Network.Wai.Middleware.Static (staticPolicy, noDots, addBase, (>->))

import Controllers


main = do
  svg <- readFile "./static/header.svg"
  scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "static")
  home svg
  elfeck
  error404 svg

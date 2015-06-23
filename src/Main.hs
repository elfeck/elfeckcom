{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Logger
import qualified Data.Text as T
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Web.Spock.Safe hiding (head, SessionId)
import Database.Persist.Sqlite hiding (get)

import Web.Utils
import Web.HandlerGet
import Web.HandlerPost
import Web.PostParser
import Model.Model


main :: IO ()
main = do
  configFile <- readFile "config.txt"
  let config = parseConfig $ T.pack configFile
  pool <- runNoLoggingT $ createSqlitePool (database config) 5
  runNoLoggingT $ runSqlPool (runMigration migrateCore) pool
  runSpock 3000 $ spock sessConfig (PCPool pool) config (app config)
    where sessConfig =
            SessionCfg { sc_cookieName = "elfeckcom"
                       , sc_sessionTTL = 60 * 5 * 50
                       , sc_sessionExpandTTL = True
                       , sc_sessionIdEntropy = 40
                       , sc_emptySession = Nothing
                       , sc_persistCfg = Nothing
                       }

app :: SiteConfig -> BlogApp
app (SiteConfig _ routes) = do
  middleware (staticPolicy (addBase "static"))
  handleGets routes
  handlePosts

{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Control.Monad.Logger
import qualified Data.Text as T
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Web.Spock.Safe hiding (head, SessionId)
import Database.Persist.Sqlite hiding (get)

import Web.Utils
import Web.GetHandler
import Web.PostHandler
import Model.Types


main :: IO ()
main = do
  args <- getArgs
  configFile <- readFile $ findConfigFile args
  let config = parseConfig $ T.pack configFile
  pool <- runNoLoggingT $ createSqlitePool (database config) 5
  runNoLoggingT $ runSqlPool (runMigration migrateCore) pool
  runSpock 3000 $ spock (spockConfig config pool) (app config)
    where spockConfig config pool = SpockCfg {
            spc_initialState = config
            , spc_database = PCPool pool
            , spc_sessionCfg = sessConfig
            , spc_maxRequestSize = Just (5 * 1024 * 1024)
            }
          sessConfig =
            SessionCfg { sc_cookieName = "elfeckcom"
                       , sc_sessionTTL = 60 * 5 * 50
                       , sc_sessionExpandTTL = True
                       , sc_sessionIdEntropy = 40
                       , sc_emptySession = Nothing
                       , sc_persistCfg = Nothing
                       , sc_housekeepingInterval = 60 * 10
                       , sc_hooks = defaultSessionHooks
                       }
          findConfigFile [] = "config.txt"
          findConfigFile arg = head arg


app :: SiteConfig -> BlogApp
app (SiteConfig rootDir _ filesDir routes) = do
  middleware (staticPolicy (addBase $ T.unpack rootDir))
  handleGets routes (T.unpack rootDir)
  handlePosts (T.unpack filesDir)

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Types where

import Data.Time
import qualified Data.Text as T
import Database.Persist.TH

type SystemSites = [(T.Text, T.Text)]
share [mkPersist sqlSettings, mkMigrate "migrateCore"][persistLowerCase|
Session
  validUntil UTCTime
  userId UserId
  deriving Show
User
  name T.Text
  pass T.Text
  salt T.Text
  access Int
  UniqueUsername name
  deriving Show
Post json
  title T.Text Maybe
  categories [T.Text] Maybe
  content T.Text
  crtDate UTCTime
  modDate UTCTime
  ptype Int
  access Int
  deriving Show
PostToCategory
  post PostId
  category T.Text
  deriving Show
SystemVisit json
  name T.Text Maybe
  region T.Text
  sites SystemSites
  crtDate UTCTime
  author T.Text
  deriving Show
|]

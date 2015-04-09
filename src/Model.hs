{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import Control.Monad.Logger
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Data.Text as T
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateCore"][persistLowerCase|
Session
  validUntil UTCTime
  userId Int
  deriving Show
Post
  title T.Text Maybe
  categories [T.Text] Maybe
  content T.Text
  modDate UTCTime
  crtDate UTCTime
  ptype Int
  access Int
|]

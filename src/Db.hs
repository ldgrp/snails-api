{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Db where

import Control.Monad.IO.Class
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time
import Servant

import Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  name Text
  username Text
  created_at UTCTime default=CURRENT_DATE
  deriving Show Eq
Entry json
  content Text
  author UserId
  created_at UTCTime default=CURRENT_DATE
  reply_to EntryId Maybe
  is_deleted Bool default=FALSE
  deriving Show Eq
EntryLikes json
  userId UserId
  entryId EntryId
  UniqueUserLike userId entryId
  deriving Show Eq
Message json
  content Text
  sender UserId
  recipient UserId
  created_at UTCTime
  deriving Show Eq
News json
  title Text
  content Text
  image_url Text
  url Text
  created_at UTCTime default=CURRENT_DATE
  deriving Show Eq
Vehicle json
  name Text
  code Text
  eta Int
  deriving Show Eq
Board json
  latitude Rational
  longitude Rational
  description Text
  deriving Show Eq
|]

doMigrations :: IO ()
doMigrations = runSqlite "db.sqlite" $ runMigration migrateAll

-- Run a database operation, and lift the result into a Handler.
-- This minimises usage of IO operations in other functions
runDb :: SqlPersistM a -> Handler a
runDb query = liftIO $ runSqlite "db.sqlite" query
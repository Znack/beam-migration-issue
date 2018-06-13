{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeApplications #-}

module Schema.Migrations.V0002ExampleBlog
  ( module Schema.Migrations.V0001ExampleBlog
  , module Schema.Migrations.V0002ExampleBlog
  ) where

import qualified Schema.Migrations.V0001ExampleBlog as V0001 hiding
  ( PrimaryKey(UserId)
  )
import Schema.Migrations.V0001ExampleBlog hiding
  ( DemoblogDb(..)
  , PrimaryKey(UserId)
  , User
  , UserId
  , UserT(..)
  , migration
  )

import Data.Text (Text)
import Data.Time (LocalTime)

import Database.Beam
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Database.Beam.Migrate
import Database.Beam.Postgres

data UserT f = User
  { _userId :: Columnar f (SqlSerial Int)
  , _userName :: Columnar f Text
  , _userCreatedAt :: Columnar f LocalTime
  } deriving (Generic, Beamable)

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f (SqlSerial Int))
                        deriving (Generic, Beamable)
  primaryKey = UserId . _userId

--
-- === DATABASE DEFINITON ===
--
data DemoblogDb user f = DemoblogDb
  { _user :: f (TableEntity UserT)
  , _post :: f (TableEntity (PostT user))
  } deriving (Generic)

instance Database Postgres (DemoblogDb UserT)

currentDb :: CheckedDatabaseSettings Postgres (DemoblogDb UserT)
currentDb = defaultMigratableDbSettings @PgCommandSyntax

migration ::
     CheckedDatabaseSettings Postgres (V0001.DemoblogDb UserT)
  -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres (DemoblogDb UserT))
migration oldDb =
  DemoblogDb <$> alterUserTable <*> preserve (V0001._post oldDb)
  where
    alterUserTable = alterTable (V0001._user oldDb) tableMigration
    tableMigration oldTable =
      User (V0001._userId oldTable) (V0001._userName oldTable) <$>
      addColumn (field "created_at" timestamptz (defaultTo_ now_) notNull)

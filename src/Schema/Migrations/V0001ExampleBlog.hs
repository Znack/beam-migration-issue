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
{-# LANGUAGE UndecidableInstances #-}

module Schema.Migrations.V0001ExampleBlog where

import Data.Text (Text)
import Data.Time (LocalTime)
import GHC.Generics

import Database.Beam
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Database.Beam.Migrate
import Database.Beam.Postgres
import Database.Beam.Schema.Tables

data UserT f = User
  { _userId :: Columnar f (SqlSerial Int)
  , _userName :: Columnar f Text
  } deriving (Generic, Beamable)

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f (SqlSerial Int))
                        deriving (Generic, Beamable)
  primaryKey = UserId . _userId

data PostT user f = Post
  { _postId :: Columnar f (SqlSerial Int)
  , _postContent :: Columnar f Text
  , _postAuthor :: PrimaryKey user f
  } deriving (Generic)

instance (Beamable (PrimaryKey user)) => Beamable (PostT user)

instance (Typeable user, Beamable (PrimaryKey user)) => Table (PostT user) where
  data PrimaryKey (PostT user) f = PostId (Columnar f
                                           (SqlSerial Int))
                               deriving (Generic, Beamable)
  primaryKey = PostId . _postId

--
-- === DATABASE DEFINITON ===
--
data DemoblogDb user f = DemoblogDb
  { _user :: f (TableEntity UserT)
  , _post :: f (TableEntity (PostT user))
  } deriving (Generic)

instance (Typeable user, Beamable (PrimaryKey user)) => Database Postgres (DemoblogDb user)

migration :: forall user. (Typeable user, Beamable (PrimaryKey user)) =>
     ()
  -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres (DemoblogDb user))
migration () =
  DemoblogDb <$>
  createTable
    "user"
    (User (field "user_id" serial) (field "name" (varchar (Just 255)) notNull)) <*>
  createTable
    "post"
    (Post
       (field "post_id" serial)
       (field "content" text notNull)
       (UserId (field "user_id" smallint)))

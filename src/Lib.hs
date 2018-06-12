module Lib
    ( selectPosts
    , createPost
    ) where

import Database.Beam
import Database.Beam.Postgres
import Schema.Database
import qualified Database.Beam.Backend.SQL.BeamExtensions as BeamExtensions

selectPosts :: Pg [(PostT UserT) Identity]
selectPosts = runSelectReturningList $ select $ all_ (_post db)

createPost content userId =
  BeamExtensions.runInsertReturningList (_post db) $
  insertExpressions
    [Post default_ (val_ content) (UserId $ fromIntegral userId)]

module Lib
    ( createPost
    ) where

import Database.Beam
import Schema.Database
import qualified Database.Beam.Backend.SQL.BeamExtensions as BeamExtensions


createPost content userId =
  BeamExtensions.runInsertReturningList (_post db) $
  insertExpressions
    [Post default_ (val_ content) (UserId $ fromIntegral userId)]

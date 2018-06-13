{-# LANGUAGE OverloadedStrings #-}

module Schema.Database 
  ( module Schema.Migrations.V0002ExampleBlog
  , module Schema.Database
  ) where

import Control.Arrow
import qualified Data.Text as T
import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Postgres
import Database.Beam.Migrate.Backend
import Database.Beam.Migrate.Generics
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres.Migrate
import qualified Database.PostgreSQL.Simple as Pg

import qualified Schema.Migrations.V0001ExampleBlog as V0001 
import qualified Schema.Migrations.V0002ExampleBlog as V0002
import Schema.Migrations.V0002ExampleBlog hiding (migration)

migration ::
     MigrationSteps PgCommandSyntax () (CheckedDatabaseSettings Postgres (DemoblogDb UserT))
migration =
  migrationStep "Add user and post tables" V0001.migration >>>
  migrationStep "Add field created_at to user table" V0002.migration

db :: DatabaseSettings Postgres (DemoblogDb UserT)
db = unCheckDatabase (evaluateDatabase migration)


verboseHooks :: BringUpToDateHooks Pg
verboseHooks =
  BringUpToDateHooks
  { runIrreversibleHook = pure True
  , startStepHook =
      \a b -> liftIO (print $ "startStepHook N" ++ show a ++ ": " ++ show b)
  , endStepHook =
      \a b -> liftIO (print $ "endStepHook N" ++ show a ++ ": " ++ show b)
  , runCommandHook =
      \a b -> liftIO (print $ "runCommandHook N" ++ show a ++ ": " ++ show b)
  , queryFailedHook = fail "Log entry query fails"
  , discontinuousMigrationsHook =
      \ix ->
        fail ("Discontinuous migration log: missing migration at " ++ show ix)
  , logMismatchHook =
      \ix actual expected ->
        fail
          ("Log mismatch at index " ++
           show ix ++
           ":\n" ++
           "  expected: " ++
           T.unpack expected ++ "\n" ++ "  actual  : " ++ T.unpack actual)
  , databaseAheadHook =
      \aheadBy ->
        fail
          ("The database is ahead of the known schema by " ++
           show aheadBy ++ " migration(s)")
  }
  
createPgConn =
  Pg.connectPostgreSQL "postgresql://demoblog@localhost:5432/beam_demoblog"

runDB query conn = runBeamPostgresDebug putStrLn conn query

migrateWithHooks conn =
  runBeamPostgres
    conn
    (bringUpToDateWithHooks verboseHooks migrationBackend migration)

runMigrations = createPgConn >>= migrateWithHooks

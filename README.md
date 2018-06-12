# beam-migration-issue
Demonstration of the problem with foreign key to outdated table 
Problem discussion: https://github.com/tathougies/beam/issues/253

# Short description
Beam now has a problem with migrations if one changes the table (add column for example) and there was some dependant tables.
Example provided where we have User and Post tables, Post has a link to User, then we add column to user.
The problem now with link from Post to User, because it leads to outdated version of User.
As a solution suggested by @tathougies I have parameterized Post (see src/Schema/Migrations/V0001ExampleBlog.hs).
Now Beam generate invalid SQL for the parameterized Post entities.

# Reproduce
Start PostgreSQL server, connect with `psql postgres` and then in psql shell:

```
CREATE DATABASE beam_demoblog;
CREATE USER demoblog;
GRANT ALL PRIVILEGES ON DATABASE beam_demoblog TO demoblog;
```


Then start GHCi repl with `stack repl` and run:
```
λ> runMigrations  -- this will create all tables in database
"startStepHook N0: \"Add user and post tables\""
"runCommandHook N0: \"CREATE TABLE \\\"user\\\" (\\\"user_id\\\" SERIAL, \\\"name\\\" VARCHAR(255) NOT NULL, PRIMARY KEY(\\\"user_id\\\")) ;\""
"runCommandHook N0: \"CREATE TABLE \\\"post\\\" (\\\"post_id\\\" SERIAL, \\\"content\\\" TEXT NOT NULL, \\\"user_id\\\" SMALLINT, PRIMARY KEY(\\\"post_id\\\")) ;\""
"endStepHook N0: \"Add user and post tables\""
"startStepHook N1: \"Add field created_at to user table\""
"runCommandHook N1: \"ALTER TABLE \\\"user\\\" ADD COLUMN \\\"created_at\\\" TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL;\""
"endStepHook N1: \"Add field created_at to user table\""
λ> createPgConn >>= runDB (createPost "Some author" 1)
INSERT INTO "post"("id", "content", "author__id") VALUES (DEFAULT, 'Some author', 1)  RETURNING "id", "content", "author__id"
*** Exception: SqlError {sqlState = "42703", sqlExecStatus = FatalError, sqlErrorMsg = "column \"id\" of relation \"post\" does not exist", sqlErrorDetail = "", sqlErrorHint = ""}
λ> createPgConn >>= runDB selectPosts
SELECT "t0"."id" AS "res0", "t0"."content" AS "res1", "t0"."author__id" AS "res2" FROM "post" AS "t0"
*** Exception: SqlError {sqlState = "42703", sqlExecStatus = FatalError, sqlErrorMsg = "column t0.id does not exist", sqlErrorDetail = "", sqlErrorHint = ""}
```

We can see that both `INSERT` and `SELECT` commands were generated with `"id"` field although in migrations we have defined `post_id`. 
Notice that migrations itself have generated the correct DDL commands and database after applying the migrations will have column `post_id` in `post` table.
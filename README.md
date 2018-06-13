# beam-migration-issue
Demonstration of the problem with foreign key to outdated table 
Problem discussion: https://github.com/tathougies/beam/issues/253

# Short description
Beam now has a problem with migrations if one changes the table (add column for example) and there was some dependant tables.
Example provided where we have User and Post tables, Post has a link to User, then we add column to user.
The problem now with link from Post to User, because it leads to outdated version of User.
As a solution suggested by @tathougies I have parameterized Post (see src/Schema/Migrations/V0001ExampleBlog.hs).
Now there is no way to compose migrations together.

# Reproduce
Start PostgreSQL server, connect with `psql postgres` and then in psql shell:

```
CREATE DATABASE beam_demoblog;
CREATE USER demoblog;
GRANT ALL PRIVILEGES ON DATABASE beam_demoblog TO demoblog;
```


Then start GHCi repl with `stack repl` and compilation should fail:
```
$ stack repl

[1 of 5] Compiling Schema.Migrations.V0001ExampleBlog ( src/Schema/Migrations/V0001ExampleBlog.hs, interpreted )

src/Schema/Migrations/V0001ExampleBlog.hs:64:3: error:
    • Couldn't match type ‘user’ with ‘UserT’
      ‘user’ is a rigid type variable bound by
        the type signature for:
          migration :: forall (user :: (* -> *) -> *).
                       (Typeable user, Beamable (PrimaryKey user)) =>
                       ()
                       -> Migration
                            PgCommandSyntax
                            (CheckedDatabaseSettings Postgres (DemoblogDb user))
        at src/Schema/Migrations/V0001ExampleBlog.hs:(60,1)-(62,83)
      Expected type: Migration
                       PgCommandSyntax
                       (CheckedDatabaseSettings Postgres (DemoblogDb user))
        Actual type: free-5.0.2:Control.Monad.Free.Church.F
                       (MigrationF PgCommandSyntax)
                       (DemoblogDb
                          UserT (CheckedDatabaseEntity Postgres (DemoblogDb user)))
    • In the expression:
        DemoblogDb
          <$>
            createTable
              "user"
              (User
                 (field "user_id" serial)
                 (field "name" (varchar (Just 255)) notNull))
          <*>
            createTable
              "post"
              (Post
                 (field "post_id" serial)
                 (field "content" text notNull)
                 (UserId (field "user_id" smallint)))
      In an equation for ‘migration’:
          migration ()
            = DemoblogDb
                <$>
                  createTable
                    "user"
                    (User
                       (field "user_id" serial)
                       (field "name" (varchar (Just 255)) notNull))
                <*>
                  createTable
                    "post"
                    (Post
                       (field "post_id" serial)
                       (field "content" text notNull)
                       (UserId (field "user_id" smallint)))
    • Relevant bindings include
        migration :: ()
                     -> Migration
                          PgCommandSyntax
                          (CheckedDatabaseSettings Postgres (DemoblogDb user))
          (bound at src/Schema/Migrations/V0001ExampleBlog.hs:63:1)
   |
64 |   DemoblogDb <$>
   |   ^^^^^^^^^^^^^^...
```

This error is quite predictable: we have to use concrete Primary Key of the UserT entity for `createTable`, 
but the whole first migration function should be polymorphic. So we had to have some way to declare foreign key
to the polymorphic entity. See the `migration` function in `src/Schema/Migrations/V0001ExampleBlog.hs`
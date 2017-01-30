# Paladin

Paladin is the back end for gizmo.gg.

Paladin analyzes Rocket League replays. It is a Haskell web server that you can
upload replays to. It will parse and analyze those replays. It provides an API
for getting the analysis.

-   [Install](#install)
-   [Configure](#configure)
-   [Migrate](#migrate)
-   [Run](#run)
-   [Develop](#develop)

## Install

You must install [PostgreSQL][] (version 9.6.1) before building Paladin. After
you install PostgreSQL, you'll need [Stack][] (version 1.3.2) to build Paladin.
Once you've got both of those things, you can build Paladin with this command:

``` sh
stack build
```

## Configure

Configuration is not necessary. Paladin provides reasonable defaults for all
its options.

Paladin can be configured with environment variables. Setting environment
variables is different depending on which operating system you're using.

-   For PowerShell on Windows, you can set environment variables with `$env:`.
    For example:

    ``` powershell
    $env:A = "1"
    echo apple $env:A
    $env:B = "2"
    echo "apple and banana"
    ```

-   For the Command Prompt (`cmd.exe`) on Windows, you can set environment
    variables with `setx`. For example:

    ``` cmd
    setx A "1"
    echo apple %A%
    setx B "2"
    echo "apple and banana"
    ```

-   For Unlix-like operating systems such as macOS and Ubuntu, you can set
    environment variables for a single command with `env`. For example:

    ``` sh
    env A=1 echo apple $A
    env A=1 B=2 echo "apple and banana"
    ```

These are the environment variables that Paladin uses:

- **PALADIN_DIRECTORY**: Defaults to `data`. This is the directory that Paladin
  uses to store things. In particular, it stores uploads in the `uploads`
  subdirectory. So by default that's `data/uploads`.

- **PALADIN_DATABASE**: Defaults to the empty string. This is how to connect to
  the PostgreSQL database. Although there is [exhaustive documentation][]
  about what this field accepts, normally you only need a few things:
  `host=localhost port=5432 user=paladin password=secret dbname=paladin`.

- **PALADIN_PORT**: Defaults to `8080`. This is the port that Paladin's web
  server will run on.

- **PALADIN_CONNECT**: Defaults to `http://localhost:8080`. This is the full
  URL that Paladin is available at. You will need to change this if you changed
  the port or if Paladin is proxied behind some other HTTP server like nginx.
  This option can have a path component, but be sure not to include a trailing
  slash. For example, `http://example.com/paladin`.

- **PALADIN_MIGRATE**: Defaults to `True`. Set this to `False` to avoid running
  database migrations when starting the server.

## Migrate

It used to be that you had to run the migrations manually. Now they are run
automatically when the server starts. If for whatever reason you want to run
the migrations without starting the server, follow the directions below.

Before the server can start, the database tables need to be created. You only
need to do this once, before the first time you start the server. Be sure that
all the database environment variables are set correctly before running these
commands.

``` sh
stack build postgresql-simple-migration-0.1.7.0
stack exec migrate init $PALADIN_DATABASE
stack exec migrate migrate $PALADIN_DATABASE migrations
```

Note that the `$PALADIN_DATABASE` environment variable should be the same as
when you run the server. On Windows, it will look a little different. In the
Command Prompy, it will be `%PALADIN_DATABASE`. In PowerShell, it will be
`$env:PALADIN_DATABASE`.

## Run

Assuming everything above went well, you should be able to start the Paladin
server with:

``` sh
stack exec paladin
```

That should print a message to the console when it starts up. Then you can go
to <http://localhost:8080> to see the server in action.

## Develop

All of the actual Haskell source files live in `source/library`, so start there
if you want to change how the application behaves. To recompile whenever
something changes, run this command:

``` sh
stack build --fast --file-watch --pedantic --test
```

You'll need the database to be both configured and migrated in order to run the
tests. If you just want to build the project, add `--no-run-tests` to the
above command.

Note that it won't run a server for you. Getting that to work (on Windows
especially) is annoying. Another option that might be slightly better is to run
this command:

``` sh
stack ghci
```

That will start a REPL. You can type `:reload` (or just `:r`) to pick up
changes after you make them. Then type `:main` (or just `:ma`) to run the
server.

If you need to change how the database looks, you'll need to add a migration to
the `migrations` folder. Follow the naming format of the files in there
already, namely `yyyy-mm-dd-HH-MM-SS-short-description.sql`.

[PostgreSQL]: https://www.postgresql.org
[Stack]: https://docs.haskellstack.org/en/stable/README/
[exhaustive documentation]: https://www.stackage.org/haddock/lts-7.17/postgresql-simple-0.5.2.1/Database-PostgreSQL-Simple.html#v:connectPostgreSQL

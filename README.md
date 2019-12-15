psql_migration
=====

An escript to help with Postgres migrations.

This is inspired by the similarly named [pgsql_migration](https://github.com/artemeff/pgsql_migration) and the excellent tooling offered by [diesel](http://diesel.rs).

Build
-----

    $ ./rebar3 escriptize

Run
---

The script will look for a `DATABASE_URL` environment variable for connectivity details a running Postgres and suports  `.env` file to hold that variable.


Run the script with:

    $ _build/default/bin/psql_migration

See the help output for more details on options and commands.

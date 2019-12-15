psql_migration
=====

[![Build status](https://badge.buildkite.com/2a17b5561e84e0ad2ce19cac2d079d5ab8c59d647389de3287.svg)](https://buildkite.com/helium/psql-migration)

An escript to help with Postgres migrations.

This is inspired by the similarly named [pgsql_migration](https://github.com/artemeff/pgsql_migration) and the excellent tooling offered by [diesel](http://diesel.rs).

Build
-----

    $ Make

Run
---

The script will look for a `DATABASE_URL` environment variable for connectivity details a running Postgres and supports  `.env` file to hold that variable.


Run the script with:

    $ _build/default/bin/psql_migration

See the help output for more details on options and commands.

# MigrateCLI

MigrateCLI is a dead-simple migration CLI for PostgreSQL.

## Installation

Currently, MigrateCLI can only be installed from source:
0. Ensure that you have [Stack](https://docs.haskellstack.org/en/stable/) installed.
1. Clone this repository.
2. `stack install`.
3. All done.

## Basic Usage

In order to create a new migration:
```
$ mgcli new name_of_migration
```

This will create a folder containing two files, `up.sql` and `down.sql`. 
Inside the former, the SQL for the migration can be written.
The latter should reverse this migration to the former state.

To run all (pending) migrations:
```
$ mgcli migrate
```

To undo the last migration:
```
$ mgcli revert
```

To revert all migrations and re-run them from the ground up:
```
$ mgcli refresh
```

## Credentials

In order to perform migrations, credentials are required.
These can be specified with `--host`, `--port`, `--database`, `--user` and `--password`.
For any of these not specified, the values of the environment variables `PGHOST`, `PGPORT`, `PGDATABASE`, `PGUSER`, `PGPASSWORD` will be used, respectively.
Should any of these be empty, the following hardcoded values will be used, respectively: `localhost`, `5432`, `postgres`, `postgres`, *(empty string)*.

By default, it will attempt to load a `.env` file present in the directory in which it was called. In most cases, it should not be necessary to manually specify the credentials

## Flags

A custom migration directory can be specified using `--dir <DIR>`.

To not load the `.env` automatically, `--no-env` may be used.
# MigrateCLI

MigrateCLI is a dead-simple migration CLI for PostgreSQL.

## 📦 Installation

Currently, MigrateCLI can only be installed from source:
0. Ensure that you have [Stack](https://docs.haskellstack.org/en/stable/) installed.
1. Clone this repository.
2. `stack install`.
3. The `mgcli` binary should now be in your PATH (`~/.local/bin` on macOS).

## 🚀 Usage

In order to create a new migration:
```
$ mgcli new name_of_migration
```

This will create a folder containing two files, `up.sql` and `down.sql`. 
The former contains the SQL to perform the migration.
The latter should reverse this migration to the former state.
Neither `up.sql` nor `down.sql` may be an empty query.

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

To see the status of the current state:
```
$ mgcli status
```

### 🪪 Credentials

In order to perform migrations, credentials are required.
These can be specified with `--host`, `--port`, `--database`, `--user` and `--password`.
For any of these not specified, the values of the environment variables `PGHOST`, `PGPORT`, `PGDATABASE`, `PGUSER`, `PGPASSWORD` will be used, respectively.
Should any of these be empty, the following hardcoded values will be used, respectively: `localhost`, `5432`, `postgres`, `postgres`, *(empty string)*.

By default, it will attempt to load a `.env` file present in the directory in which it was called. In most cases, it should not be necessary to manually specify the credentials. When a variable is present both as a normal environment variable and within a `.env`, the former will take priority.

### 🏁 Flags

A custom migration directory can be specified using `--dir <DIR>`.

To not load the `.env` automatically, `--no-dotenv` may be used.

## ⚖️ License

Copyright Paul Hübner (c) 2023

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Author name here nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
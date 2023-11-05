{-# LANGUAGE OverloadedStrings #-}

module Queries
    ( queryAddMigration
    , queryCreateMigrationsTable
    , queryMigrations
    , queryRemoveMigration
    , str2Query
    ) where

import qualified Data.ByteString.Char8            as Char8
import           Database.PostgreSQL.Simple.Types

queryCreateMigrationsTable :: Query
queryCreateMigrationsTable = "set client_min_messages = error; create table if not exists migratecli_migrations (id serial primary key, name text unique not null, stamp timestamp not null);"

queryMigrations :: Query
queryMigrations = "select id, name from migratecli_migrations;"

queryAddMigration :: Query
queryAddMigration = "insert into migratecli_migrations (name, stamp) values (?, ?);"

queryRemoveMigration :: Query
queryRemoveMigration = "delete from migratecli_migrations where name = ?;"

str2Query :: String -> Query
str2Query = Query . Char8.pack

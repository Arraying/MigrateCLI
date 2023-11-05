{-# LANGUAGE ScopedTypeVariables #-}

module Database
    ( getPerformedMigrations
    , initializeTable
    , runMigrationDown
    , runMigrationUp
    ) where

import           Config
import qualified Data.ByteString.Char8      as Char8
import qualified Data.Text                  as Text
import           Data.Time.Clock            (getCurrentTime)
import           Database.PostgreSQL.Simple (Connection, Only (..), close,
                                             connectPostgreSQL, execute,
                                             execute_, query_, withTransaction)
import           Queries                    (queryAddMigration,
                                             queryCreateMigrationsTable,
                                             queryMigrations,
                                             queryRemoveMigration, str2Query)

initializeTable :: Configuration -> IO ()
initializeTable cfg = do
  conn <- cx cfg
  _ <- execute_ conn queryCreateMigrationsTable
  close conn


getPerformedMigrations :: Configuration -> IO [String]
getPerformedMigrations cfg = do
  conn <- cx cfg
  res :: [(Int, Text.Text)] <- query_ conn queryMigrations
  close conn
  return $ fmap (Text.unpack . snd) res

runMigrationUp :: Configuration -> String -> String -> IO ()
runMigrationUp cfg name sql = do
  now <- getCurrentTime
  conn <- cx cfg
  _ <- withTransaction conn (do
    _ <- execute_ conn $ str2Query sql
    _ <- execute conn queryAddMigration (name, now)
    return ())
  close conn

runMigrationDown :: Configuration -> String -> String -> IO ()
runMigrationDown cfg name sql = do
  conn <- cx cfg
  _ <- withTransaction conn (do
    _ <- execute_ conn $ str2Query sql
    _ <- execute conn queryRemoveMigration $ Only name
    return ())
  close conn

cx :: Configuration -> IO Connection
cx cfg = connectPostgreSQL $ Char8.pack $ makeConnectionString cfg

makeConnectionString :: Configuration -> String
makeConnectionString c =
  "postgresql://" ++ username c ++ ":" ++ password c ++ "@" ++ host c ++ ":" ++ show (port c) ++ "/" ++ database c

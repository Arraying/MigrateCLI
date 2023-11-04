module Lib
  ( Configuration(..)
  , Command(..)
  , runMigrateCLI
  ) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import System.Directory
import System.FilePath

data Configuration = Configuration
  { host     :: String
  , port     :: Int
  , database :: String
  , username :: String
  , password :: String
  , dir      :: String
  , noEnv    :: Bool
  , runCmd   :: Command }

data Command
  = New String
  | Migrate
  | Revert
  | Refresh

runMigrateCLI :: Configuration -> IO ()
runMigrateCLI config = do
  case runCmd config of
    (New str) -> addMigration str $ dir config
    Migrate   -> putStrLn $ "Migrating"
    Revert    -> putStrLn $ "Revert"
    Refresh   -> putStrLn $ "Refresh"

addMigration :: String -> String -> IO ()
addMigration name path = do
  _ <- createDirectoryIfMissing True path
  n <- makeMigrationName name
  putStrLn n
  let d = path </> n
  collision <- doesDirectoryExist d
  case collision of
    True -> putStrLn "Migration with this name already exists, please re-run the command in a few seconds"
    False -> do
      _ <- createDirectory d
      writeFile (d </> "up.sql") "-- The SQL to perform the migration:\n"
      writeFile (d </> "down.sql") "-- The SQL to revert the migration:\n"
      putStrLn $ "Successfully created migration " ++ n

makeMigrationName :: String -> IO String
makeMigrationName name = do
  (y, mo, d, h, m, s) <- date
  return $ y ++ "_" ++ mo ++ "_" ++ d ++ "_" ++ h ++ m ++ s ++ "_" ++ name 


date :: IO (String, String, String, String, String, String)
date = do
  time <- getCurrentTime
  let (y, mo, d) = toGregorian $ utctDay time
  let t = timeToTimeOfDay (utctDayTime time)
  return (show y, show mo, show d, show $ todHour t, show $ todMin t, show $ todSec t)
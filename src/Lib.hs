module Lib
  ( Configuration(..)
  , Command(..)
  , runMigrateCLI
  ) where

import qualified Data.List as List
import Config
import Control.Monad
import Data.Function
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import System.Directory
import System.FilePath
import Database

runMigrateCLI :: Configuration -> IO ()
runMigrateCLI config = do
  case runCmd config of
    (New str) -> addMigration str $ dir config
    Migrate   -> do
      initializeTable config
      performMigration config
    Revert    -> revertMigration config
    Refresh   -> putStrLn $ "Refresh"

addMigration :: String -> String -> IO ()
addMigration name path = do
  _ <- createDirectoryIfMissing True path
  n <- makeMigrationName name
  putStrLn n
  let d = path </> n
  collision <- doesDirectoryExist d
  (if collision then putStrLn "Migration with this name already exists, please re-run the command in a few seconds"
  else (do
    _ <- createDirectory d
    writeFile (d </> "up.sql") "-- The SQL to perform the migration:\n"
    writeFile (d </> "down.sql") "-- The SQL to revert the migration:\n"
    putStrLn $ "Successfully created migration " ++ n))

performMigration :: Configuration -> IO ()
performMigration cfg = do
  putStrLn "Running all pending migrations..."
  performed <- getPerformedMigrations cfg
  defined <- getDefinedMigrations cfg
  let diff = List.sortBy (compare `on` snd) $ filter (\(x, _) -> x `notElem` performed) defined
  mapM_ (performIndividualMigration cfg) diff
  putStrLn "Done"

performIndividualMigration :: Configuration -> (String, FilePath) -> IO ()
performIndividualMigration cfg (name, path) = do
  sql <- readFile $ path </> "up.sql"
  _ <- runMigrationUp cfg name sql
  putStrLn $ "+ " ++ name

revertMigration :: Configuration -> IO ()
revertMigration cfg = do
  performed <- getPerformedMigrations cfg
  if null performed then putStrLn "No known migrations, cannot revert"
  else (do
    let hd = maximum performed
    putStrLn "Reverting most recent migration..."
    revertIndividualMigration cfg (hd, dir cfg </> hd))


revertIndividualMigration :: Configuration -> (String, FilePath) -> IO ()
revertIndividualMigration cfg (name, path) = do
  sql <- readFile $ path </> "down.sql"
  _ <- runMigrationDown cfg name sql
  putStrLn $ "- " ++ name


getDefinedMigrations :: Configuration -> IO [(String, FilePath)]
getDefinedMigrations cfg = do
  inFolder <- listDirectory $ dir cfg
  let paths = fmap (\n -> (n, dir cfg </> n)) inFolder
  filterM (doesDirectoryExist . snd) paths

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
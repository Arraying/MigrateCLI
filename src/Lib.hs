module Lib
    ( Command (..)
    , Configuration (..)
    , runMigrateCLI
    ) where

import           Config
import           Control.Monad
import           Data.Function
import qualified Data.List           as List
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Database
import           System.Directory
import           System.FilePath

runMigrateCLI :: Configuration -> IO ()
runMigrateCLI config = do
  case runCmd config of
    Status    -> status config
    (New str) -> addMigration str $ dir config
    Migrate   -> do
      initializeTable config
      performMigration config
    Revert    -> revertMigration config
    Refresh   -> refreshMigrations config

status :: Configuration -> IO ()
status cfg = do
  (success, localButNotMigrated, migratedButNotLocal) <- getSummary cfg
  unless (null success) (do
    putStrLn "Performed migrations:"
    mapM_ (\x -> putStrLn $ "* " ++ x) success)
  unless (null localButNotMigrated) (do
      putStrLn "Pending migrations:"
      mapM_ (\x -> putStrLn $ "- " ++ x) $ fmap fst localButNotMigrated)
  unless (null migratedButNotLocal) (do
    putStrLn "Migrations in the database but unknown to this project:"
    mapM_ (\x -> putStrLn $ "+ " ++ x) migratedButNotLocal)

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
  (_, diff, _) <- getSummary cfg
  mapM_ (performIndividualMigration cfg) diff
  putStrLn "Done"

performIndividualMigration :: Configuration -> (String, FilePath) -> IO ()
performIndividualMigration cfg (name, path) = do
  sql <- readFile $ path </> "up.sql"
  _ <- runMigrationUp cfg name sql
  putStrLn $ "+ " ++ name

revertMigration :: Configuration -> IO ()
revertMigration cfg = do
  (success, _, _) <- getSummary cfg
  if null success then putStrLn "No known migrations, cannot revert"
  else (do
    let hd = maximum success
    putStrLn "Reverting most recent migration..."
    revertIndividualMigration cfg (hd, dir cfg </> hd)
    putStrLn "Done")

revertIndividualMigration :: Configuration -> (String, FilePath) -> IO ()
revertIndividualMigration cfg (name, path) = do
  sql <- readFile $ path </> "down.sql"
  _ <- runMigrationDown cfg name sql
  putStrLn $ "- " ++ name

refreshMigrations :: Configuration -> IO ()
refreshMigrations cfg = do
  (successRaw, _, _) <- getSummary cfg
  let success = reverse successRaw
  putStrLn "Reversing all existing migrations..."
  mapM_ (\x -> revertIndividualMigration cfg (x, dir cfg </> x)) success
  putStrLn "Re-running all migrations..."
  (_, pending, _) <- getSummary cfg
  mapM_ (performIndividualMigration cfg) pending
  putStrLn "Done"

getSummary :: Configuration -> IO ([String], [(String, FilePath)], [String])
getSummary cfg = do
  performed <- getPerformedMigrations cfg
  defined <- getDefinedMigrations cfg
  let success = List.sort $ performed `List.intersect` fmap fst defined
  let localButNotMigrated = List.sortBy (compare `on` snd) $ filter (\(x, _) -> x `notElem` performed) defined
  let migratedButNotLocal = List.sort $ filter (\x -> x `notElem` fmap fst defined) performed
  return (success, localButNotMigrated, migratedButNotLocal)

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

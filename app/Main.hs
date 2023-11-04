module Main (main) where

import Configuration.Dotenv
import Lib
import Options.Applicative
import Text.Read
import System.Environment

parseOptions :: Parser Configuration
parseOptions = Configuration
  <$> strOption 
     ( long "host"
    <> metavar "HOST"
    <> value ""
    <> help "The host (excl. port)" )
  <*> option auto
     ( long "port" 
    <> metavar "PORT"
    <> value 0
    <> help "The port number" )
  <*> strOption
     ( long "database"
    <> metavar "DB"
    <> value ""
    <> help "The database name" )
  <*> strOption
     ( long "user"
    <> metavar "USERNAME"
    <> value ""
    <> help "The database username" )
  <*> strOption
     ( long "password"
    <> metavar "PASSWORD"
    <> value ""
    <> help "The database password" )
  <*> strOption
     ( long "dir"
    <> metavar "DIRECTORY"
    <> value "."
    <> help "The migrations directory" )
  <*> switch
     ( long "no-dotenv"
    <> help "Whether to NOT load the .env in the cwd" )
  <*> subparser
     ( command "add" (info (Add <$> argument str 
       ( metavar "NAME" 
      <> help "The name of the migration" )) idm)
    <> command "migrate" (info (pure Migrate) idm)
    <> command "revert" (info (pure Revert) idm) 
    <> command "refresh" (info (pure Refresh) idm) )

run :: Configuration -> IO ()
run (Configuration h p d usr pwd dir nv cmd) = do
  _ <- 
    if not nv then onMissingFile (loadFile dotEnvConfig) (return ()) 
    else return ()
  h' <- resolveHost h
  p' <- resolvePort p
  d' <- resolveDatabase d
  usr' <- resolveUser usr
  pwd' <- resolvePassword pwd
  let config = Configuration h' p' d' usr' pwd' dir nv cmd
  return ()

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> header "MigrateCLI - A PostgreSQL migration command line tool" )

resolveHost :: String -> IO String
resolveHost "" = envOrBackup "PGHOST" "localhost"
resolveHost x = return x

resolvePort :: Int -> IO Int
resolvePort 0 = envOrBackup' "PGPORT" 5432
resolvePort x = return x

resolveDatabase :: String -> IO String
resolveDatabase "" = envOrBackup "PGDATABASE" "postgres"
resolveDatabase x = return x

resolveUser :: String -> IO String
resolveUser "" = envOrBackup "PGUSER" "postgres"
resolveUser x = return x

resolvePassword :: String -> IO String
resolvePassword "" = envOrBackup "PGPASSWORD" ""
resolvePassword x = return x

envOrBackup :: String -> String -> IO String
envOrBackup name def = do
  val <- lookupEnv name
  return $ case val of
    (Just v) -> v
    Nothing  -> def

envOrBackup' :: String -> Int -> IO Int
envOrBackup' name def = do
  val <- lookupEnv name
  return $ case val >>= (\v -> readMaybe v :: Maybe Int) of
    (Just v) -> v
    Nothing  -> def

dotEnvConfig :: Config
dotEnvConfig = Config
  { configExamplePath = []
  , configOverride = False
  , configPath = [ ".env" ]
  , configVerbose = False
  , allowDuplicates = True
  }
module Main (main) where

import Lib
import Options.Applicative

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
     ( long "no-env"
    <> help "Whether to NOT load the .env in the cwd" )

run :: Configuration -> IO ()
run (Configuration host port database user password dir noEnv) = return ()

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> header "MigrateCLI - A PostgreSQL migration command line tool" )

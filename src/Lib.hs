module Lib
  ( Configuration(..)
  , Command(..)
  , someFunc
  ) where

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
  = Add String
  | Migrate
  | Revert
  | Refresh

someFunc :: IO ()
someFunc = putStrLn "migrate cli"
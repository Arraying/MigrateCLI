module Config
  ( Configuration(..)
  , Command(..) ) where

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
  = Status
  | New String
  | Migrate
  | Revert
  | Refresh
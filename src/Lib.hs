module Lib
  ( Configuration(..)
  , someFunc
  ) where

data Configuration = Configuration
  { host     :: String
  , port     :: Int
  , database :: String
  , username :: String
  , password :: String
  , dir      :: String
  , noEnv    :: Bool }

someFunc :: IO ()
someFunc = putStrLn "migrate cli"
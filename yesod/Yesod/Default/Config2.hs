{-# LANGUAGE OverloadedStrings #-}

-- | Some next-gen helper functions for the scaffolding's configuration system.
module Yesod.Default.Config2
  ( -- * Locally defined
    configSettingsYml
  , getDevSettings
  , develMainHelper
  , makeYesodLogger

    -- * Re-exports from Data.Yaml.Config
  , applyCurrentEnv
  , getCurrentEnv
  , applyEnvValue
  , loadYamlSettings
  , loadYamlSettingsArgs
  , EnvUsage
  , ignoreEnv
  , useEnv
  , requireEnv
  , useCustomEnv
  , requireCustomEnv

    -- * For backwards compatibility
  , MergedValue (..)
  , loadAppSettings
  , loadAppSettingsArgs
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Data.Aeson
import qualified Data.Aeson.KeyMap as H
import Data.Maybe (fromMaybe)
import Data.Yaml.Config
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Network.Wai.Logger (clockDateCacher)
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.Exit (exitSuccess)
import System.Log.FastLogger (LoggerSet)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT)
import Text.Read (readMaybe)
import Yesod.Core.Types (Logger (Logger))

newtype MergedValue = MergedValue {getMergedValue :: Value}

instance Semigroup MergedValue where
  MergedValue x <> MergedValue y = MergedValue $ mergeValues x y

-- | Left biased
mergeValues :: Value -> Value -> Value
mergeValues (Object x) (Object y) = Object $ H.unionWith mergeValues x y
mergeValues x _ = x

-- | Load the settings from the following three sources:
--
-- * Run time config files
--
-- * Run time environment variables
--
-- * The default compile time config file
loadAppSettings ::
  (FromJSON settings) =>
  -- | run time config files to use, earlier files have precedence
  [FilePath] ->
  -- | any other values to use, usually from compile time config. overridden by files
  [Value] ->
  EnvUsage ->
  IO settings
loadAppSettings = loadYamlSettings
{-# DEPRECATED loadAppSettings "Use loadYamlSettings" #-}

-- | Same as @loadAppSettings@, but get the list of runtime config files from
-- the command line arguments.
loadAppSettingsArgs ::
  (FromJSON settings) =>
  -- | any other values to use, usually from compile time config. overridden by files
  [Value] ->
  -- | use environment variables
  EnvUsage ->
  IO settings
loadAppSettingsArgs = loadYamlSettingsArgs
{-# DEPRECATED loadAppSettingsArgs "Use loadYamlSettingsArgs" #-}

-- | Location of the default config file.
configSettingsYml :: FilePath
configSettingsYml = "config/settings.yml"

-- | Helper for getApplicationDev in the scaffolding. Looks up PORT and
-- DISPLAY_PORT and prints appropriate messages.
getDevSettings :: Settings -> IO Settings
getDevSettings settings = do
  env <- getEnvironment
  let p = fromMaybe (getPort settings) $ lookup "PORT" env >>= readMaybe
      pdisplay = fromMaybe p $ lookup "DISPLAY_PORT" env >>= readMaybe
  putStrLn $ "Devel application launched: http://localhost:" ++ show pdisplay
  pure $ setPort p settings

-- | Helper for develMain in the scaffolding.
develMainHelper :: IO (Settings, Application) -> IO ()
develMainHelper getSettingsApp = do
  _ <- installHandler sigINT (Catch $ pure ()) Nothing

  putStrLn "Starting devel application"
  (settings, app) <- getSettingsApp
  _ <- forkIO $ runSettings settings app
  loop
 where
  loop :: IO ()
  loop = do
    threadDelay 100000
    e <- doesFileExist "yesod-devel/devel-terminate"
    if e then terminateDevel else loop

  terminateDevel :: IO ()
  terminateDevel = exitSuccess

-- | Create a 'Logger' value (from yesod-core) out of a 'LoggerSet' (from
-- fast-logger).
makeYesodLogger :: LoggerSet -> IO Logger
makeYesodLogger loggerSet' = do
  (getter, _) <- clockDateCacher
  pure $! Yesod.Core.Types.Logger loggerSet' getter

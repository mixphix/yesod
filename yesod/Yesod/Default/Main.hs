{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Yesod.Default.Main
  ( defaultMain
  , defaultMainLog
  , defaultRunner
  , defaultDevelApp
  , LogFunc
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (when)
import Control.Monad.Logger (Loc, LogLevel (LevelError), LogSource, liftLoc)
import Data.Maybe (fromMaybe)
import Language.Haskell.TH.Syntax (qLocation)
import Network.Wai (Application)
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setHost
  , setOnException
  , setPort
  )
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Gzip
  ( GzipFiles (GzipCacheFolder)
  , defaultGzipSettings
  , gzip
  , gzipFiles
  )
import Network.Wai.Middleware.Jsonp (jsonp)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.Environment (getEnvironment)
import System.Log.FastLogger (LogStr, toLogStr)
import qualified System.Posix.Signals as Signal
import Text.Read (readMaybe)
import Yesod.Default.Config

-- | Run your app, taking environment and port settings from the
--   commandline.
--
--   @'fromArgs'@ helps parse a custom configuration
--
--   > main :: IO ()
--   > main = defaultMain (fromArgs parseExtra) makeApplication
defaultMain ::
  IO (AppConfig env extra) ->
  (AppConfig env extra -> IO Application) ->
  IO ()
defaultMain load getApp = do
  config <- load
  app <- getApp config
  runSettings
    ( setPort (appPort config) $
        setHost (appHost config) defaultSettings
    )
    app

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

-- | Same as @defaultMain@, but gets a logging function back as well as an
-- @Application@ to install Warp exception handlers.
--
-- Since 1.2.5
defaultMainLog ::
  IO (AppConfig env extra) ->
  (AppConfig env extra -> IO (Application, LogFunc)) ->
  IO ()
defaultMainLog load getApp = do
  config <- load
  (app, logFunc) <- getApp config
  runSettings
    ( setPort (appPort config) $
        setHost (appHost config) $
          setOnException
            ( const $ \e ->
                when (shouldLog' e) $
                  logFunc
                    $(qLocation >>= liftLoc)
                    "yesod"
                    LevelError
                    (toLogStr $ "Exception from Warp: " ++ show e)
            )
            defaultSettings
    )
    app
 where
  shouldLog' = Warp.defaultShouldDisplayException

-- | Run your application continuously, listening for SIGINT and exiting
--   when received
--
--   > withYourSite :: AppConfig DefaultEnv -> Logger -> (Application -> IO a) -> IO ()
--   > withYourSite conf logger f = do
--   >     Settings.withConnectionPool conf $ \p -> do
--   >         runConnectionPool (runMigration yourMigration) p
--   >         defaultRunner f $ YourSite conf logger p
defaultRunner :: (Application -> IO ()) -> Application -> IO ()
defaultRunner f app = do
  -- clear the .static-cache so we don't have stale content
  exists <- doesDirectoryExist staticCache
  when exists $ removeDirectoryRecursive staticCache
  tid <- forkIO $ f (middlewares app)
  flag <- newEmptyMVar
  _ <-
    Signal.installHandler
      Signal.sigINT
      ( Signal.CatchOnce $ do
          putStrLn "Caught an interrupt"
          killThread tid
          putMVar flag ()
      )
      Nothing
  takeMVar flag
 where
  middlewares = gzip gset . jsonp . autohead

  gset = defaultGzipSettings{gzipFiles = GzipCacheFolder staticCache}
  staticCache = ".static-cache"

-- | Run your development app using a custom environment type and loader
--   function
defaultDevelApp ::
  -- | A means to load your development @'AppConfig'@
  IO (AppConfig env extra) ->
  -- | Get your @Application@
  (AppConfig env extra -> IO Application) ->
  IO (Int, Application)
defaultDevelApp load getApp = do
  conf <- load
  env <- getEnvironment
  let p = fromMaybe (appPort conf) $ lookup "PORT" env >>= readMaybe
      pdisplay = fromMaybe p $ lookup "DISPLAY_PORT" env >>= readMaybe
  putStrLn $ "Devel application launched: http://localhost:" ++ show pdisplay
  app <- getApp conf
  pure (p, app)

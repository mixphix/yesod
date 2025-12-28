{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Yesod.Core
  ( -- * Type classes
    Yesod (..)
  , YesodDispatch (..)
  , YesodSubDispatch (..)
  , RenderRoute (..)
  , ParseRoute (..)
  , RouteAttrs (..)

    -- ** Breadcrumbs
  , YesodBreadcrumbs (..)
  , breadcrumbs

    -- * Types
  , Approot (..)
  , FileUpload (..)
  , ErrorResponse (..)

    -- * Utilities
  , maybeAuthorized
  , widgetToPageContent

    -- * Defaults
  , defaultErrorHandler
  , defaultYesodMiddleware
  , authorizationCheck

    -- * Data types
  , AuthResult (..)
  , unauthorizedI

    -- * Logging
  , defaultMakeLogger
  , defaultMessageLoggerSource
  , defaultShouldLogIO
  , formatLogMessage
  , LogLevel (..)
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logOther
  , logDebugS
  , logInfoS
  , logWarnS
  , logErrorS
  , logOtherS

    -- * Sessions
  , SessionBackend (..)
  , customizeSessionCookies
  , defaultClientSessionBackend
  , envClientSessionBackend
  , clientSessionBackend
  , sslOnlySessions
  , laxSameSiteSessions
  , strictSameSiteSessions
  , sslOnlyMiddleware
  , clientSessionDateCacher
  , loadClientSession
  , Header (..)

    -- * CSRF protection
  , defaultCsrfMiddleware
  , defaultCsrfSetCookieMiddleware
  , csrfSetCookieMiddleware
  , defaultCsrfCheckMiddleware
  , csrfCheckMiddleware

    -- * JS loaders
  , ScriptLoadPosition (..)
  , BottomOfHeadAsync

    -- * Generalizing type classes
  , MonadHandler (..)
  , MonadWidget (..)

    -- * Approot
  , guessApproot
  , guessApprootOr
  , getApprootText

    -- * Misc
  , yesodVersion
  , yesodRender
  , Yesod.Core.runFakeHandler

    -- * LiteApp
  , module Yesod.Core.Internal.LiteApp

    -- * Low-level
  , yesodRunner

    -- * Re-exports
  , module Yesod.Core.Content
  , module Yesod.Core.Dispatch
  , module Yesod.Core.Handler
  , module Yesod.Core.Widget
  , module Yesod.Core.Json
  , module Text.Shakespeare.I18N
  , module Yesod.Core.Internal.Util
  , module Text.Blaze.Html
  , MonadTrans (..)
  , MonadIO (..)
  , MonadUnliftIO (..)
  , MonadResource (..)
  , MonadLogger

    -- * Commonly referenced functions/datatypes
  , Application

    -- * Utilities
  , showIntegral
  , readIntegral

    -- * Shakespeare

    -- ** Hamlet
  , hamlet
  , shamlet
  , xhamlet
  , HtmlUrl

    -- ** Julius
  , julius
  , JavascriptUrl
  , renderJavascriptUrl

    -- ** Cassius/Lucius
  , cassius
  , lucius
  , CssUrl
  , renderCssUrl
  ) where

import Text.Blaze.Html (Html, preEscapedToMarkup, toHtml)
import Text.Shakespeare.I18N
import Yesod.Core.Class.Handler
import Yesod.Core.Content
import Yesod.Core.Dispatch
import Yesod.Core.Handler
import Yesod.Core.Internal.Util (formatRFC1123, formatRFC822, formatW3)
import Yesod.Core.Json
import Yesod.Core.Types
import Yesod.Core.Widget

import Control.Monad.Logger
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Version (showVersion)
import qualified Paths_yesod_core
import UnliftIO (MonadIO (..), MonadUnliftIO (..))
import Yesod.Core.Class.Breadcrumbs
import Yesod.Core.Class.Dispatch
import Yesod.Core.Class.Yesod
import Yesod.Core.Internal.Run (yesodRender, yesodRunner)
import qualified Yesod.Core.Internal.Run
import Yesod.Core.Internal.Session
import Yesod.Routes.Class

import Control.Monad.Trans.Resource (MonadResource (..))
import Network.Wai (Application)
import Text.Cassius
import Text.Hamlet
import Text.Julius
import Text.Lucius
import Yesod.Core.Internal.LiteApp

runFakeHandler ::
  (Yesod site, MonadIO m) =>
  SessionMap ->
  (site -> Logger) ->
  site ->
  HandlerFor site a ->
  m (Either ErrorResponse a)
runFakeHandler = Yesod.Core.Internal.Run.runFakeHandler
{-# DEPRECATED runFakeHandler "import runFakeHandler from Yesod.Core.Unsafe" #-}

-- | Return an 'Unauthorized' value, with the given i18n message.
unauthorizedI ::
  (MonadHandler m, RenderMessage (HandlerSite m) msg) => msg -> m AuthResult
unauthorizedI msg = do
  mr <- getMessageRender
  pure $ Unauthorized $ mr msg

yesodVersion :: String
yesodVersion = showVersion Paths_yesod_core.version

-- | Return the same URL if the user is authorized to see it.
--
-- Built on top of 'isAuthorized'. This is useful for building page that only
-- contain links to pages the user is allowed to see.
maybeAuthorized ::
  (Yesod site) =>
  Route site ->
  -- | is this a write request?
  Bool ->
  HandlerFor site (Maybe (Route site))
maybeAuthorized r isWrite = do
  x <- isAuthorized r isWrite
  pure $ if x == Authorized then Just r else Nothing

showIntegral :: (Integral a) => a -> String
showIntegral x = show (fromIntegral x :: Integer)

readIntegral :: (Num a) => String -> Maybe a
readIntegral s =
  case reads s of
    (i, _) : _ -> Just $ fromInteger i
    [] -> Nothing

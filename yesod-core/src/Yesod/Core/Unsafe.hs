-- | This is designed to be used as
--
-- > import qualified Yesod.Core.Unsafe as Unsafe
--
-- This serves as a reminder that the functions are unsafe to use in many situations.
module Yesod.Core.Unsafe (runFakeHandler, fakeHandlerGetLogger) where

import Yesod.Core.Internal.Run (runFakeHandler)

import Control.Monad.IO.Class (MonadIO)
import Yesod.Core.Class.Yesod
import Yesod.Core.Types

-- | designed to be used as
--
-- > unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
fakeHandlerGetLogger ::
  (Yesod site, MonadIO m) =>
  (site -> Logger) ->
  site ->
  HandlerFor site a ->
  m a
fakeHandlerGetLogger getLogger app f =
  runFakeHandler mempty getLogger app f
    >>= either
      (error . ("runFakeHandler issue: " <>) . show)
      return

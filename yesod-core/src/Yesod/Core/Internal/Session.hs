module Yesod.Core.Internal.Session
  ( encodeClientSession
  , decodeClientSession
  , clientSessionDateCacher
  , ClientSessionDateCache (..)
  , SaveSession
  , SessionBackend (..)
  ) where

import Control.AutoUpdate
import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.Serialize
import Data.Time
import qualified Web.ClientSession as CS
import Yesod.Core.Internal.Util
import Yesod.Core.Types

encodeClientSession ::
  CS.Key ->
  CS.IV ->
  -- | expire time
  ClientSessionDateCache ->
  -- | remote host
  ByteString ->
  -- | session
  SessionMap ->
  -- | cookie value
  ByteString
encodeClientSession key iv date rhost session' =
  CS.encrypt key iv $ encode $ SessionCookie expires rhost session'
 where
  expires = Right (csdcExpiresSerialized date)

decodeClientSession ::
  CS.Key ->
  -- | current time
  ClientSessionDateCache ->
  -- | remote host field
  ByteString ->
  -- | cookie value
  ByteString ->
  Maybe SessionMap
decodeClientSession key date rhost encrypted = do
  decrypted <- CS.decrypt key encrypted
  SessionCookie (Left expire) rhost' session' <-
    either (const Nothing) Just $ decode decrypted
  guard $ expire > csdcNow date
  guard $ rhost' == rhost
  return session'

----------------------------------------------------------------------

-- Originally copied from Kazu's date-cache, but now using mkAutoUpdate.
--
-- The cached date is updated every 10s, we don't need second
-- resolution for session expiration times.
--
-- The second component of the returned tuple used to be an action that
-- killed the updater thread, but is now a no-op that's just there
-- to preserve the type.

clientSessionDateCacher ::
  -- | Inactive session validity.
  NominalDiffTime ->
  IO (IO ClientSessionDateCache, IO ())
clientSessionDateCacher validity = do
  getClientSessionDateCache <-
    mkAutoUpdate
      defaultUpdateSettings
        { updateAction = getUpdated
        , updateFreq = 10000000 -- 10s
        }

  return (getClientSessionDateCache, return ())
 where
  getUpdated = do
    now <- getCurrentTime
    let expires = validity `addUTCTime` now
        expiresS = runPut (putTime expires)
    return $! ClientSessionDateCache now expires expiresS

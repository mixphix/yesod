{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | NOTE: Mozilla Persona will be shut down by the end of 2016, therefore this
-- module is no longer recommended for use.
module Yesod.Auth.BrowserId
  {-# DEPRECATED "Mozilla Persona will be shut down by the end of 2016" #-}
  ( authBrowserId
  , createOnClick
  , createOnClickOverride
  , def
  , BrowserIdSettings
  , bisAudience
  , bisLazyLoad
  , forwardUrl
  ) where

import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.Default
import Data.FileEmbed (embedFile)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (parseURI, uriPath)
import Text.Julius (rawJS)
import Web.Authenticate.BrowserId
import Yesod.Auth
import Yesod.Core

pid :: Text
pid = "browserid"

forwardUrl :: AuthRoute
forwardUrl = PluginR pid []

complete :: AuthRoute
complete = forwardUrl

-- | A settings type for various configuration options relevant to BrowserID.
--
-- See: <http://www.yesodweb.com/book/settings-types>
--
-- Since 1.2.0
data BrowserIdSettings = BrowserIdSettings
  { bisAudience :: Maybe Text
  -- ^ BrowserID audience value. If @Nothing@, will be extracted based on the
  -- approot.
  --
  -- Default: @Nothing@
  --
  -- Since 1.2.0
  , bisLazyLoad :: Bool
  -- ^ Use asynchronous Javascript loading for the BrowserID JS file.
  --
  -- Default: @True@.
  --
  -- Since 1.2.0
  }

instance Default BrowserIdSettings where
  def =
    BrowserIdSettings
      { bisAudience = Nothing
      , bisLazyLoad = True
      }

authBrowserId :: (YesodAuth m) => BrowserIdSettings -> AuthPlugin m
authBrowserId bis@BrowserIdSettings{..} =
  AuthPlugin
    { apName = pid
    , apDispatch = \m ps ->
        case (m, ps) of
          ("GET", [assertion]) -> do
            audience <-
              case bisAudience of
                Just a -> pure a
                Nothing -> do
                  r <- getUrlRender
                  tm <- getRouteToParent
                  pure $ T.takeWhile (/= '/') $ stripScheme $ r $ tm LoginR
            manager <- authHttpManager
            memail <- checkAssertion audience assertion manager
            case memail of
              Nothing -> do
                $logErrorS "yesod-auth" "BrowserID assertion failure"
                tm <- getRouteToParent
                loginErrorMessage (tm LoginR) "BrowserID login error."
              Just email ->
                setCredsRedirect
                  Creds
                    { credsPlugin = pid
                    , credsIdent = email
                    , credsExtra = []
                    }
          ("GET", ["static", "sign-in.png"]) ->
            sendResponse
              ( "image/png" :: ByteString
              , toContent
                  $(embedFile "/Users/mbrown/Documents/Code/Haskell/yesod/yesod-auth/persona_sign_in_blue.png")
              )
          (_, []) -> badMethod
          _ -> notFound
    , apLogin = \toMaster -> do
        onclick <- createOnClick bis toMaster

        autologin <- (== Just "true") <$> lookupGetParam "autologin"
        when autologin $ toWidget [julius|#{rawJS onclick}();|]

        toWidget
          [hamlet|
$newline never
<p>
    <a href="javascript:#{onclick}()">
        <img src=@{toMaster loginIcon}>
|]
    }
 where
  loginIcon = PluginR pid ["static", "sign-in.png"]
  stripScheme t = fromMaybe t $ T.stripPrefix "//" $ snd $ T.breakOn "//" t

-- | Generates a function to handle on-click events, and returns that function
-- name.
createOnClickOverride ::
  BrowserIdSettings ->
  (Route Auth -> Route master) ->
  Maybe (Route master) ->
  WidgetFor master Text
createOnClickOverride BrowserIdSettings{..} toMaster mOnRegistration = do
  unless bisLazyLoad $ addScriptRemote browserIdJs
  onclick <- newIdent
  render <- getUrlRender
  let login = toJSON $ getPath $ render loginRoute -- (toMaster LoginR)
      loginRoute = fromMaybe (toMaster LoginR) mOnRegistration
  toWidget
    [julius|
        function #{rawJS onclick}() {
            if (navigator.id) {
                navigator.id.watch({
                    onlogin: function (assertion) {
                        if (assertion) {
                            document.location = "@{toMaster complete}/" + assertion;
                        }
                    },
                    onlogout: function () {}
                });
                navigator.id.request({
                    returnTo: #{login} + "?autologin=true"
                });
            }
            else {
                alert("Loading, please try again");
            }
        }
    |]
  when bisLazyLoad $
    toWidget
      [julius|
        (function(){
            var bid = document.createElement("script");
            bid.async = true;
            bid.src = #{toJSON browserIdJs};
            var s = document.getElementsByTagName('script')[0];
            s.parentNode.insertBefore(bid, s);
        })();
    |]

  autologin <- (== Just "true") <$> lookupGetParam "autologin"
  when autologin $ toWidget [julius|#{rawJS onclick}();|]
  pure onclick
 where
  getPath t = fromMaybe t $ do
    uri <- parseURI $ T.unpack t
    pure $ T.pack $ uriPath uri

-- | Generates a function to handle on-click events, and returns that function
-- name.
createOnClick ::
  BrowserIdSettings ->
  (Route Auth -> Route master) ->
  WidgetFor master Text
createOnClick bidSettings toMaster = createOnClickOverride bidSettings toMaster Nothing

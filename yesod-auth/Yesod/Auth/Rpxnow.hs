{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Yesod.Auth.Rpxnow
  ( authRpxnow
  ) where

import Control.Monad (mplus)
import qualified Web.Authenticate.Rpxnow as Rpxnow
import Yesod.Auth

import Control.Arrow ((***))
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Network.HTTP.Types (renderQuery)
import Yesod.Core

authRpxnow ::
  (YesodAuth master) =>
  -- | app name
  String ->
  -- | key
  String ->
  AuthPlugin master
authRpxnow app apiKey =
  AuthPlugin "rpxnow" dispatch login
 where
  login tm = do
    render <- getUrlRender
    let queryString =
          decodeUtf8With lenientDecode $
            renderQuery
              True
              [("token_url", Just $ encodeUtf8 $ render $ tm $ PluginR "rpxnow" [])]
    toWidget
      [hamlet|
$newline never
<iframe src="http://#{app}.rpxnow.com/openid/embed#{queryString}" scrolling="no" frameBorder="no" allowtransparency="true" style="width:400px;height:240px">
|]

  dispatch :: a -> [b] -> AuthHandler master TypedContent
  dispatch _ [] = do
    token1 <- lookupGetParams "token"
    token2 <- lookupPostParams "token"
    token <- case token1 ++ token2 of
      [] -> invalidArgs ["token: Value not supplied"]
      x : _ -> pure $ unpack x
    manager <- authHttpManager
    Rpxnow.Identifier ident extra <- Rpxnow.authenticate apiKey token manager
    let creds =
          Creds "rpxnow" ident
            $ maybe
              id
              (\x -> (:) ("verifiedEmail", x))
              (lookup "verifiedEmail" extra)
            $ maybe
              id
              (\x -> (:) ("displayName", pack x))
              (getDisplayName $ map (unpack *** unpack) extra)
              []
    setCredsRedirect creds
  dispatch _ _ = notFound

-- | Get some form of a display name.
getDisplayName :: [(String, String)] -> Maybe String
getDisplayName extra =
  foldr (\x -> mplus (lookup x extra)) Nothing choices
 where
  choices = ["verifiedEmail", "email", "displayName", "preferredUsername"]

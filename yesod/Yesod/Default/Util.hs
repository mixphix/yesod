{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Various utilities used in the scaffolded site.
module Yesod.Default.Util
  ( addStaticContentExternal
  , globFile
  , globFilePackage
  , widgetFileNoReload
  , widgetFileReload
  , TemplateLanguage (..)
  , defaultTemplateLanguages
  , WidgetFileSettings
  , wfsLanguages
  , wfsHamletSettings
  ) where

-- purposely using complete import so that Haddock will see addStaticContent

import Conduit
import Control.Monad (unless, when)
import qualified Data.ByteString.Lazy as L
import Data.Default.Class (Default (def))
import Data.Either (fromRight)
import Data.FileEmbed (makeRelativeToProject)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import Language.Haskell.TH.Syntax hiding
  ( makeRelativeToProject
  )
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Text.Cassius (cassiusFile, cassiusFileReload)
import Text.Hamlet (HamletSettings, defaultHamletSettings)
import Text.Julius (juliusFile, juliusFileReload)
import Text.Lucius (luciusFile, luciusFileReload)
import Yesod.Core

-- | An implementation of 'addStaticContent' which stores the contents in an
-- external file. Files are created in the given static folder with names based
-- on a hash of their content. This allows expiration dates to be set far in
-- the future without worry of users receiving stale content.
addStaticContentExternal ::
  -- | javascript minifier
  (L.ByteString -> Either a L.ByteString) ->
  -- | hash function to determine file name
  (L.ByteString -> String) ->
  -- | location of static directory. files will be placed within a "tmp" subfolder
  FilePath ->
  -- | route constructor, taking a list of pieces
  ([Text] -> Route master) ->
  -- | filename extension
  Text ->
  -- | mime type
  Text ->
  -- | file contents
  L.ByteString ->
  HandlerFor master (Maybe (Either Text (Route master, [(Text, Text)])))
addStaticContentExternal minify hash staticDir toRoute ext' _ content = do
  liftIO $ createDirectoryIfMissing True statictmp
  exists <- liftIO $ doesFileExist fn'
  unless exists $ withSinkFileCautious fn' $ \sink ->
    runConduit $ sourceLazy content' .| sink
  pure $ Just $ Right (toRoute ["tmp", pack fn], [])
 where
  fn, statictmp, fn' :: FilePath
  -- by basing the hash off of the un-minified content, we avoid a costly
  -- minification if the file already exists
  fn = hash content ++ '.' : unpack ext'
  statictmp = staticDir ++ "/tmp/"
  fn' = statictmp ++ fn

  content' :: L.ByteString
  content'
    | ext' == "js" = fromRight content $ minify content
    | otherwise = content

-- | expects a file extension for each type, e.g: hamlet lucius julius
globFile :: String -> String -> FilePath
globFile kind x = "templates/" ++ x ++ "." ++ kind

-- | `globFile` but returned path is absolute and within the package the Q Exp is evaluated
-- @since 1.6.1.0
globFilePackage :: String -> String -> Q FilePath
globFilePackage = (makeRelativeToProject <$>) . globFile

data TemplateLanguage = TemplateLanguage
  { tlRequiresToWidget :: Bool
  , tlExtension :: String
  , tlNoReload :: FilePath -> Q Exp
  , tlReload :: FilePath -> Q Exp
  }

defaultTemplateLanguages :: HamletSettings -> [TemplateLanguage]
defaultTemplateLanguages hset =
  [ TemplateLanguage False "hamlet" whamletFile' whamletFile'
  , TemplateLanguage True "cassius" cassiusFile cassiusFileReload
  , TemplateLanguage True "julius" juliusFile juliusFileReload
  , TemplateLanguage True "lucius" luciusFile luciusFileReload
  ]
 where
  whamletFile' = whamletFileWithSettings hset

data WidgetFileSettings = WidgetFileSettings
  { wfsLanguages :: HamletSettings -> [TemplateLanguage]
  , wfsHamletSettings :: HamletSettings
  }

instance Default WidgetFileSettings where
  def = WidgetFileSettings defaultTemplateLanguages defaultHamletSettings

widgetFileNoReload :: WidgetFileSettings -> FilePath -> Q Exp
widgetFileNoReload wfs x =
  combine "widgetFileNoReload" x False $ wfsLanguages wfs $ wfsHamletSettings wfs

widgetFileReload :: WidgetFileSettings -> FilePath -> Q Exp
widgetFileReload wfs x = combine "widgetFileReload" x True $ wfsLanguages wfs $ wfsHamletSettings wfs

combine :: String -> String -> Bool -> [TemplateLanguage] -> Q Exp
combine func file isReload tls = do
  mexps <- qmexps
  case catMaybes mexps of
    [] ->
      error $
        concat
          [ "Called "
          , func
          , " on "
          , show file
          , ", but no templates were found."
          ]
    exps -> pure $ DoE Nothing $ map NoBindS exps
 where
  qmexps :: Q [Maybe Exp]
  qmexps = mapM go tls

  go :: TemplateLanguage -> Q (Maybe Exp)
  go tl =
    whenExists
      file
      (tlRequiresToWidget tl)
      (tlExtension tl)
      ((if isReload then tlReload else tlNoReload) tl)

whenExists ::
  String ->
  -- | requires toWidget wrap
  Bool ->
  String ->
  (FilePath -> Q Exp) ->
  Q (Maybe Exp)
whenExists = warnUnlessExists False

warnUnlessExists ::
  Bool ->
  String ->
  -- | requires toWidget wrap
  Bool ->
  String ->
  (FilePath -> Q Exp) ->
  Q (Maybe Exp)
warnUnlessExists shouldWarn x wrap glob f = do
  fn <- globFilePackage glob x
  e <- qRunIO $ doesFileExist fn
  when (shouldWarn && not e) $ qRunIO $ putStrLn $ "widget file not found: " ++ fn
  if e
    then do
      ex <- f fn
      if wrap
        then do
          tw <- [|toWidget|]
          pure $ Just $ tw `AppE` ex
        else pure $ Just ex
    else pure Nothing

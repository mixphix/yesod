{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Core.Internal.TH
  ( mkYesod
  , mkYesodOpts
  , mkYesodWith
  , mkYesodData
  , mkYesodDataOpts
  , mkYesodSubData
  , mkYesodSubDataOpts
  , mkYesodWithParser
  , mkYesodWithParserOpts
  , mkYesodDispatch
  , mkYesodDispatchOpts
  , masterTypeSyns
  , mkYesodGeneral
  , mkYesodGeneralOpts
  , mkMDS
  , mkDispatchInstance
  , mkYesodSubDispatch
  , subTopDispatch
  , instanceD
  , RouteOpts
  , defaultOpts
  , setEqDerived
  , setShowDerived
  , setReadDerived
  , setCreateResources
  , setParameterizedSubroute
  )
where

import Control.Monad (replicateM, void)
import Data.ByteString.Lazy.Char8 ()
import Language.Haskell.TH hiding (cxt, instanceD)
import Language.Haskell.TH.Syntax
import qualified Network.Wai as W
import Text.Parsec (eof, many, many1, option, parse, sepBy1, try)
import Text.ParserCombinators.Parsec.Char (alphaNum, char, spaces, string)
import Yesod.Core.Class.Dispatch
import Yesod.Core.Content (ToTypedContent (..))
import Yesod.Core.Handler
import Yesod.Core.Internal.Run
import Yesod.Core.Types
import Yesod.Routes.Parse
import Yesod.Routes.TH
import Prelude hiding (exp)

-- | Generates URL datatype and site function for the given 'Resource's. This
-- is used for creating sites, /not/ subsites. See 'mkYesodSubData' and 'mkYesodSubDispatch' for the latter.
-- Use 'parseRoutes' to create the 'Resource's.
--
-- Contexts and type variables in the name of the datatype are parsed.
-- For example, a datatype @App a@ with typeclass constraint @MyClass a@ can be written as @\"(MyClass a) => App a\"@.
mkYesod ::
  -- | name of the argument datatype
  String ->
  [ResourceTree String] ->
  Q [Dec]
mkYesod = mkYesodOpts defaultOpts

-- | `mkYesod` but with custom options.
--
-- @since 1.6.25.0
mkYesodOpts ::
  RouteOpts ->
  String ->
  [ResourceTree String] ->
  Q [Dec]
mkYesodOpts opts name = fmap (uncurry (++)) . mkYesodWithParserOpts opts name False pure

{-# DEPRECATED
  mkYesodWith
  "Contexts and type variables are now parsed from the name in `mkYesod`. <https://github.com/yesodweb/yesod/pull/1366>"
  #-}

-- | Similar to 'mkYesod', except contexts and type variables are not parsed.
-- Instead, they are explicitly provided.
-- You can write @(MyClass a) => App a@ with @mkYesodWith [[\"MyClass\",\"a\"]] \"App\" [\"a\"] ...@.
mkYesodWith ::
  -- | list of contexts
  [[String]] ->
  -- | name of the argument datatype
  String ->
  -- | list of type variables
  [String] ->
  [ResourceTree String] ->
  Q [Dec]
mkYesodWith cxts name args = fmap (uncurry (++)) . mkYesodGeneral cxts name args False pure

-- | Sometimes, you will want to declare your routes in one file and define
-- your handlers elsewhere. For example, this is the only way to break up a
-- monolithic file into smaller parts. Use this function, paired with
-- 'mkYesodDispatch', to do just that.
mkYesodData :: String -> [ResourceTree String] -> Q [Dec]
mkYesodData = mkYesodDataOpts defaultOpts

-- | `mkYesodData` but with custom options.
--
-- @since 1.6.25.0
mkYesodDataOpts :: RouteOpts -> String -> [ResourceTree String] -> Q [Dec]
mkYesodDataOpts opts name resS = fst <$> mkYesodWithParserOpts opts name False pure resS

mkYesodSubData :: String -> [ResourceTree String] -> Q [Dec]
mkYesodSubData = mkYesodSubDataOpts defaultOpts

-- |
--
-- @since 1.6.25.0
mkYesodSubDataOpts :: RouteOpts -> String -> [ResourceTree String] -> Q [Dec]
mkYesodSubDataOpts opts name resS = fst <$> mkYesodWithParserOpts opts name True pure resS

-- | Parses contexts and type arguments out of name before generating TH.
mkYesodWithParser ::
  -- | foundation type
  String ->
  -- | is this a subsite
  Bool ->
  -- | unwrap handler
  (Exp -> Q Exp) ->
  [ResourceTree String] ->
  Q ([Dec], [Dec])
mkYesodWithParser = mkYesodWithParserOpts defaultOpts

-- | Parses contexts and type arguments out of name before generating TH.
--
-- @since 1.6.25.0
mkYesodWithParserOpts ::
  -- | Additional route options
  RouteOpts ->
  -- | foundation type
  String ->
  -- | is this a subsite
  Bool ->
  -- | unwrap handler
  (Exp -> Q Exp) ->
  [ResourceTree String] ->
  Q ([Dec], [Dec])
mkYesodWithParserOpts opts name isSub f resS = do
  let (name', rest, cxt) = case parse parseName "" name of
        Left err -> error $ show err
        Right a -> a
  mkYesodGeneralOpts opts cxt name' rest isSub f resS
 where
  parseName = do
    cxt <- option [] parseContext
    name' <- parseWord
    args <- many parseWord
    spaces
    eof
    pure (name', args, cxt)

  parseWord = do
    spaces
    many1 alphaNum

  parseContext = try $ do
    cxts <- parseParen parseContexts
    spaces
    _ <- string "=>"
    pure cxts

  parseParen p = do
    spaces
    _ <- char '('
    r <- p
    spaces
    _ <- char ')'
    pure r

  parseContexts =
    sepBy1 (many1 parseWord) (spaces >> char ',' >> pure ())

-- | See 'mkYesodData'.
mkYesodDispatch :: String -> [ResourceTree String] -> Q [Dec]
mkYesodDispatch = mkYesodDispatchOpts defaultOpts

-- | See 'mkYesodDataOpts'
--
-- @since 1.6.25.0
mkYesodDispatchOpts :: RouteOpts -> String -> [ResourceTree String] -> Q [Dec]
mkYesodDispatchOpts opts name = fmap snd . mkYesodWithParserOpts opts name False pure

-- | Get the Handler and Widget type synonyms for the given site.
masterTypeSyns :: [Name] -> Type -> [Dec] -- FIXME remove from here, put into the scaffolding itself?
masterTypeSyns vs site =
  [ TySynD (mkName "Handler") (fmap plainTV vs) $
      ConT ''HandlerFor `AppT` site
  , TySynD (mkName "Widget") (fmap plainTV vs) $
      ConT ''WidgetFor `AppT` site `AppT` ConT ''()
  ]

mkYesodGeneral ::
  -- | Appliction context. Used in RenderRoute, RouteAttrs, and ParseRoute instances.
  [[String]] ->
  -- | foundation type
  String ->
  -- | arguments for the type
  [String] ->
  -- | is this a subsite
  Bool ->
  -- | unwrap handler
  (Exp -> Q Exp) ->
  [ResourceTree String] ->
  Q ([Dec], [Dec])
mkYesodGeneral = mkYesodGeneralOpts defaultOpts

-- |
--
-- @since 1.6.25.0
mkYesodGeneralOpts ::
  -- | Options to adjust route creation
  RouteOpts ->
  -- | Appliction context. Used in RenderRoute, RouteAttrs, and ParseRoute instances.
  [[String]] ->
  -- | foundation type
  String ->
  -- | arguments for the type
  [String] ->
  -- | is this a subsite
  Bool ->
  -- | unwrap handler
  (Exp -> Q Exp) ->
  [ResourceTree String] ->
  Q ([Dec], [Dec])
mkYesodGeneralOpts opts appCxt' namestr mtys isSub f resS = do
  let appCxt =
        fmap
          ( \ctxs ->
              case ctxs of
                c : rest ->
                  foldl' (\acc v -> acc `AppT` fst (nameToType v)) (ConT $ mkName c) rest
                [] -> error $ "Bad context: " ++ show ctxs
          )
          appCxt'
  mname <- lookupTypeName namestr
  arity <- case mname of
    Just name -> do
      info <- reify name
      pure $
        case info of
          TyConI dec ->
            case dec of
              DataD _ _ vs _ _ _ -> length vs
              NewtypeD _ _ vs _ _ _ -> length vs
              TySynD _ vs _ -> length vs
              _ -> 0
          _ -> 0
    _ -> pure 0
  let name = mkName namestr
  -- Generate as many variable names as the arity indicates
  vns <- replicateM (arity - length mtys) $ newName "t"
  -- types that you apply to get a concrete site name
  let boundNames = fmap nameToType mtys
      argtypes = fmap fst boundNames ++ fmap VarT vns
  -- typevars that should appear in synonym head
  let argvars = (fmap mkName . filter isTvar) mtys ++ vns
  -- Base type (site type with variables)
  let site = foldl' AppT (ConT name) argtypes
      res = map (fmap (parseType . dropBracket)) resS
  renderRouteDec <- mkRenderRouteInstanceOpts opts appCxt boundNames site res
  routeAttrsDec <- mkRouteAttrsInstance appCxt site res
  dispatchDec <- mkDispatchInstance site appCxt f res
  parseRoute <- mkParseRouteInstance appCxt site res
  let rname = mkName $ "resources" ++ namestr
  resourcesDec <-
    if shouldCreateResources opts
      then do
        eres <- lift resS
        pure
          [ SigD rname $ ListT `AppT` (ConT ''ResourceTree `AppT` ConT ''String)
          , FunD rname [Clause [] (NormalB eres) []]
          ]
      else do
        pure []
  let dataDec =
        concat
          [ [parseRoute]
          , renderRouteDec
          , [routeAttrsDec]
          , resourcesDec
          , if isSub then [] else masterTypeSyns argvars site
          ]
  pure (dataDec, dispatchDec)

mkMDS :: (Exp -> Q Exp) -> Q Exp -> Q Exp -> MkDispatchSettings a site b
mkMDS f rh sd =
  MkDispatchSettings
    { mdsRunHandler = rh
    , mdsSubDispatcher = sd
    , mdsGetPathInfo = [|W.pathInfo|]
    , mdsSetPathInfo = [|\p r -> r{W.pathInfo = p}|]
    , mdsMethod = [|W.requestMethod|]
    , mds404 = [|void notFound|]
    , mds405 = [|void badMethod|]
    , mdsGetHandler = defaultGetHandler
    , mdsUnwrapper = f
    }

-- | If the generation of @'YesodDispatch'@ instance require finer
-- control of the types, contexts etc. using this combinator. You will
-- hardly need this generality. However, in certain situations, like
-- when writing library/plugin for yesod, this combinator becomes
-- handy.
mkDispatchInstance ::
  -- | The master site type
  Type ->
  -- | Context of the instance
  Cxt ->
  -- | Unwrap handler
  (Exp -> Q Exp) ->
  -- | The resource
  [ResourceTree c] ->
  DecsQ
mkDispatchInstance master cxt f res = do
  clause' <-
    mkDispatchClause
      ( mkMDS
          f
          [|yesodRunner|]
          [|
            \parentRunner getSub toParent env ->
              yesodSubDispatch
                YesodSubRunnerEnv
                  { ysreParentRunner = parentRunner
                  , ysreGetSub = getSub
                  , ysreToParentRoute = toParent
                  , ysreParentEnv = env
                  }
            |]
      )
      res
  let thisDispatch = FunD 'yesodDispatch [clause']
  pure [instanceD cxt yDispatch [thisDispatch]]
 where
  yDispatch = ConT ''YesodDispatch `AppT` master

mkYesodSubDispatch :: [ResourceTree a] -> Q Exp
mkYesodSubDispatch res = do
  clause' <-
    mkDispatchClause
      ( mkMDS
          pure
          [|subHelper|]
          [|subTopDispatch|]
      )
      res
  inner <- newName "inner"
  let innerFun = FunD inner [clause']
  helper <- newName "helper"
  let fun =
        FunD
          helper
          [ Clause
              []
              (NormalB $ VarE inner)
              [innerFun]
          ]
  pure $ LetE [fun] (VarE helper)

subTopDispatch ::
  (YesodSubDispatch sub master) =>
  ( forall content.
    (ToTypedContent content) =>
    SubHandlerFor child master content ->
    YesodSubRunnerEnv child master ->
    Maybe (Route child) ->
    W.Application
  ) ->
  (mid -> sub) ->
  (Route sub -> Route mid) ->
  YesodSubRunnerEnv mid master ->
  W.Application
subTopDispatch _ getSub toParent env =
  yesodSubDispatch
    ( YesodSubRunnerEnv
        { ysreParentRunner = ysreParentRunner env
        , ysreGetSub = getSub . ysreGetSub env
        , ysreToParentRoute = ysreToParentRoute env . toParent
        , ysreParentEnv = ysreParentEnv env
        }
    )

instanceD :: Cxt -> Type -> [Dec] -> Dec
instanceD = InstanceD Nothing

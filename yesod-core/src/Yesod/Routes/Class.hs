{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Routes.Class
  ( RenderRoute (..)
  , ParseRoute (..)
  , RouteAttrs (..)
  ) where

import Data.Set (Set)
import Data.Text (Text)

class (Eq (Route a)) => RenderRoute a where
  -- | The <http://www.yesodweb.com/book/routing-and-handlers type-safe URLs> associated with a site argument.
  data Route a

  renderRoute ::
    Route a ->
    -- | The path of the URL split on forward slashes, and a list of query parameters with their associated value.
    ([Text], [(Text, Text)])

class (RenderRoute a) => ParseRoute a where
  parseRoute ::
    -- | The path of the URL split on forward slashes, and a list of query parameters with their associated value.
    ([Text], [(Text, Text)]) ->
    Maybe (Route a)

class (RenderRoute a) => RouteAttrs a where
  routeAttrs ::
    Route a ->
    -- | A set of <http://www.yesodweb.com/book/route-attributes attributes associated with the route>.
    Set Text

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Yesod.Routes.TH.RouteAttrs
  ( mkRouteAttrsInstance
  ) where

import Data.Set (fromList)
import Data.Text (pack)
import Language.Haskell.TH.Syntax
import Yesod.Routes.Class
import Yesod.Routes.TH.Types

mkRouteAttrsInstance :: Cxt -> Type -> [ResourceTree a] -> Q Dec
mkRouteAttrsInstance cxt typ ress = do
  clauses <- mapM (goTree id) ress
  return $
    instanceD
      cxt
      (ConT ''RouteAttrs `AppT` typ)
      [ FunD 'routeAttrs $ concat clauses
      ]

goTree :: (Pat -> Pat) -> ResourceTree a -> Q [Clause]
goTree front (ResourceLeaf res) = return <$> goRes front res
goTree front (ResourceParent name _check pieces trees) =
  concat <$> mapM (goTree front') trees
 where
  ignored = (replicate toIgnore WildP ++) . return
  toIgnore = length $ filter isDynamic pieces
  isDynamic Dynamic{} = True
  isDynamic Static{} = False
  front' =
    front
      . ConP
        (mkName name)
        []
      . ignored

goRes :: (Pat -> Pat) -> Resource a -> Q Clause
goRes front Resource{..} =
  return $
    Clause
      [front $ RecP (mkName resourceName) []]
      (NormalB $ VarE 'fromList `AppE` ListE (map toText resourceAttrs))
      []
 where
  toText s = VarE 'pack `AppE` LitE (StringL s)

instanceD :: Cxt -> Type -> [Dec] -> Dec
instanceD = InstanceD Nothing

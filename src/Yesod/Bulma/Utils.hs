{-# LANGUAGE TypeFamilies #-}
module Yesod.Bulma.Utils
  ( addStylesheet'
  , addScript'
  , addBulmaResource
  )
where

import           Data.Text         (Text)
import           Yesod.Bulma.Class
import           Yesod.Core

addBulmaResource
  :: ( YesodBulma site
     , HandlerSite m ~ site
     , MonadWidget m
     )
  => m ()
addBulmaResource = do
  addStylesheet' urlBulmaCss
  addScript' urlFontawesomeJs

addStylesheet'
  :: (MonadWidget m, HandlerSite m ~ site)
  => (site -> Either (Route site) Text)
  -> m ()
addStylesheet' f = do
  y <- getYesod
  addStylesheetEither $ f y

addScript'
  :: (HandlerSite m ~ site, MonadWidget m)
  => (site -> Either (Route site) Text)
  -> m ()
addScript' f = do
  y <- getYesod
  addScriptEither $ f y

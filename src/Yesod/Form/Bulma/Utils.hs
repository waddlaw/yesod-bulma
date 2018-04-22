{-# LANGUAGE TypeFamilies #-}
module Yesod.Form.Bulma.Utils
  ( addStylesheet'
  , addScript'
  )
where

import           Data.Text  (Text)
import           Yesod.Core

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

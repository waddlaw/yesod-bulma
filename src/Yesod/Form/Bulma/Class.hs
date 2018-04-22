{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.Bulma.Class
where

import           Data.Text                                ( Text )
import           Yesod.Core

class YesodBulma a where
  urlBulmaCss :: a -> Either (Route a) Text
  urlBulmaCss _ = Right "//cdnjs.cloudflare.com/ajax/libs/bulma/0.7.0/css/bulma.min.css"

  urlBulmaExCheckRadio :: a -> Either (Route a) Text
  urlBulmaExCheckRadio _ = Right "//cdn.jsdelivr.net/npm/bulma-extensions@1.0.14/bulma-checkradio/dist/bulma-checkradio.min.css"

  urlFontawesomeJs :: a -> Either (Route a) Text
  urlFontawesomeJs _ = Right "//use.fontawesome.com/releases/v5.0.8/js/all.js"
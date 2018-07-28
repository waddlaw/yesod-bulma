{-# LANGUAGE OverloadedStrings #-}
module Yesod.Paginator.Internal
  ( renderGetParams
  , getBalancedPages
  , getUpdateGetParams
  ) where

import           Yesod.Paginator.Prelude

import           Yesod.Core
import           Yesod.Paginator.Pages

import qualified Data.Text               as T
import           Network.URI.Encode      (encodeText)

renderGetParams :: [(Text, Text)] -> Text
renderGetParams [] = ""
renderGetParams ps = "?" <> T.intercalate "&" (map renderGetParam ps)
    where renderGetParam (k, v) = encodeText k <> "=" <> encodeText v

getBalancedPages :: Natural -> Pages a -> ([PageNumber], [PageNumber])
getBalancedPages elements pages =
    if genericLength nextPages >= (elements `div` 2)
        then (prevPagesNaive, nextPages)
        else (prevPagesCalcd, nextPages)
  where
    nextPages = takeNextPages (elements - genericLength prevPagesNaive - 1) pages
    prevPagesNaive = takePreviousPages (elements `div` 2) pages
    prevPagesCalcd = takePreviousPages (elements - genericLength nextPages - 1) pages

getUpdateGetParams :: WidgetFor site (PageNumber -> [(Text, Text)])
getUpdateGetParams = do
    params <- handlerToWidget $ reqGetParams <$> getRequest
    pure $ \number -> nubOn fst $ [("p", tshow number)] <> params

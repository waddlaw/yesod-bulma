{-# LANGUAGE QuasiQuotes #-}
module Yesod.Paginator.Bulma
  ( simple
  ) where

import           Yesod.Paginator.Prelude

import           Yesod.Core
import           Yesod.Paginator.Internal
import           Yesod.Paginator.Pages
import           Yesod.Paginator.Widgets  (PaginationWidget)

simple :: Natural -> PaginationWidget m a
simple elements pages = do
  updateGetParams <- getUpdateGetParams

  let (prevPages, nextPages) = getBalancedPages elements pages
      mPrevPage = getPreviousPage pages
      mNextPage = getNextPage pages

  [whamlet|$newline never
    <ul .pagination>
      $maybe prevPage <- mPrevPage
        <li .prev>
          <a href=#{renderGetParams $ updateGetParams prevPage}>«
      $nothing
        <li .prev .disabled>
          <a>«
      $forall number <- prevPages
        <li .prev >
          <a href=#{renderGetParams $ updateGetParams number}>#{number}
      $with number <- pageNumber $ pagesCurrent pages
        <li .active .disabled>
          <a>#{number}
      $forall number <- nextPages
        <li .next>
          <a href=#{renderGetParams $ updateGetParams number}>#{number}
      $maybe nextPage <- mNextPage
        <li .next>
          <a href=#{renderGetParams $ updateGetParams nextPage}>»
      $nothing
        <li .next .disabled>
          <a>»
    |]

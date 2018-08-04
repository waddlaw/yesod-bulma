{-# LANGUAGE QuasiQuotes #-}
module Yesod.Paginator.Bulma
  ( simple
  , ellipsed
  , module X
  ) where

import           Yesod.Paginator.Prelude

import           Yesod.Core
import           Yesod.Paginator          as X hiding (ellipsed, simple)
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
    $maybe prevPage <- mPrevPage
      <a .pagination-previous href=#{renderGetParams $ updateGetParams prevPage}>«
    $nothing
      <a .pagination-previous>«
    $maybe nextPage <- mNextPage
      <a .pagination-next href=#{renderGetParams $ updateGetParams nextPage}>»
    $nothing
      <a .pagination-next>»

    <ul .pagination-list>
      $forall number <- prevPages
        <li>
          <a .pagination-link href=#{renderGetParams $ updateGetParams number}>#{number}
      $with number <- pageNumber $ pagesCurrent pages
        <li>
          <a .pagination-link .is-current>#{number}
      $forall number <- nextPages
        <li>
          <a .pagination-link href=#{renderGetParams $ updateGetParams number}>#{number}
    |]

-- | Show pages before and after, ellipsis, and first/last
ellipsed :: Natural -> PaginationWidget m a
ellipsed elements pages = do
  updateGetParams <- getUpdateGetParams

  let (prevPages, nextPages) = getBalancedPages elements pages

      mPrevPage = getPreviousPage pages
      mNextPage = getNextPage pages

      (mFirstPage, firstEllipses)
          | pageNumber (pagesCurrent pages) == 1 = (Nothing, False)
          | headMay prevPages == Just 1 = (Nothing, False)
          | headMay prevPages == Just 2 = (Just 1, False)
          | otherwise = (Just 1, True)

      (mLastPage, lastEllipses)
          | pageNumber (pagesCurrent pages) == pagesLast pages = (Nothing, False)
          | lastMay nextPages == Just (pagesLast pages) = (Nothing, False)
          | lastMay nextPages == Just (pagesLast pages - 1) = (Just $ pagesLast pages, False)
          | otherwise = (Just $ pagesLast pages, True)

  [whamlet|$newline never
    $maybe prevPage <- mPrevPage
      <a .pagination-previous href=#{renderGetParams $ updateGetParams prevPage}>«
    $nothing
      <a .pagination-previous>«
    $maybe nextPage <- mNextPage
      <a .pagination-next href=#{renderGetParams $ updateGetParams nextPage}>»
    $nothing
      <a .pagination-next>»

    <ul .pagination-list>
      $maybe firstPage <- mFirstPage
        <li>
          <a .pagination-link href=#{renderGetParams $ updateGetParams firstPage}>#{firstPage}
        $if firstEllipses
          <li>
            <span .pagination-ellipsis>
              &hellip;
      $forall number <- prevPages
        <li>
          <a .pagination-link href=#{renderGetParams $ updateGetParams number}>#{number}
      $with number <- pageNumber $ pagesCurrent pages
        <li>
          <a .pagination-link .is-current>#{number}
      $forall number <- nextPages
        <li>
          <a .pagination-link href=#{renderGetParams $ updateGetParams number}>#{number}
      $maybe lastPage <- mLastPage
        $if lastEllipses
          <li>
            <span .pagination-ellipsis>
              &hellip;
        <li>
          <a .pagination-link href=#{renderGetParams $ updateGetParams lastPage}>#{lastPage}
  |]

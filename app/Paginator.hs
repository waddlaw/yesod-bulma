{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Main (main) where

import           Yesod
import           Yesod.Form.Bulma
import           Yesod.Form.Bulma.Utils
import           Yesod.Paginator.Bulma

data App = App

instance Yesod App
instance YesodBulma App

mkYesod "App" [parseRoutes|
/ RootR GET
|]

getRootR :: Handler Html
getRootR = do
    let things' = [1..1142] :: [Int]

    pages <- paginate 3 things'

    defaultLayout $ do
      addStylesheet' urlBulmaCss
      addScript' urlFontawesomeJs
      setTitle "Paginator Example"
      [whamlet| $newline never
        <section .section>
          <div .container>
            <h1 .title>Pagination Examples
            <h2 .subtitle>The things:
            <ul>
              $forall thing <- pageItems $ pagesCurrent pages
                <li>Thing #{show thing}

            <h2 .subtitle>Simple navigation
            <nav .pagination .is-centered>
              ^{simple 10 pages}

            <h2 .subtitle>Ellipsed navigation
            <nav .pagination .is-centered>
              ^{ellipsed 10 pages}
      |]

main :: IO ()
main = warp 3001 App

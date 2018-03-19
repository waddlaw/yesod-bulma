{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Main (main) where

import           Data.Text               (Text)
import           Yesod
import           Yesod.Form.Bulma

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET POST
|]

instance Yesod App

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

data Basic = Basic
  { name     :: Text
  , username :: Text
  , email    :: Text
  , subject  :: Text
  , message  :: Textarea
  , agree    :: Bool
  , question :: Text
  }

basicForm :: Html -> MForm Handler (FormResult Basic, Widget)
basicForm =
  renderBulma BulmaBasicForm
    $   Basic
    <$> areq bulmaTextField  ("Text input" `withPlaceholder` "Name")   Nothing
    <*> areq bulmaTextField  ("bulma" `withPlaceholder` "Username")    Nothing
    <*> areq bulmaEmailField ("Email input" `withPlaceholder` "Email") Nothing
    <*> areq
          (bulmaSelectFieldList
            [("Select dropdown" :: Text, "v1"), ("With options", "vv2")]
          )
          "Subject"
          Nothing
    <*> areq bulmaTextareaField ("Textarea" `withPlaceholder` "Message") Nothing
    <*> areq (bulmaCheckBoxField "I agree to the terms and conditions")
             ""
             Nothing
    <*> areq (bulmaRadioFieldList [("yes" :: Text, "y"), ("no", "n")])
             ""
             Nothing
    <*  bulmaSubmit
          (BulmaSubmit ("保存" :: Text)
                       "btn-default"
                       [("attribute-name", "attribute-value")]
          )


getHomeR :: Handler Html
getHomeR = do
  ((result, form1), enctype) <- runFormPost basicForm

  defaultLayout $ do
    addStylesheetRemote "//cdnjs.cloudflare.com/ajax/libs/bulma/0.6.2/css/bulma.min.css"
    addScriptRemote "//use.fontawesome.com/releases/v5.0.8/js/all.js"
    case result of
      FormSuccess res ->
        [whamlet|
          <section .section .columns>
            <div .container .column .is-4>
              <form method=post action=@{HomeR} enctype=#{enctype}>
                ^{form1}

            <div .container .column .is-4>
              <table .table>
                <tr><th>Name</th><td>#{name res}</td></tr>
                <tr><th>Username</th><td>#{username res}</td></tr>
                <tr><th>Email</th><td>#{email res}</td></tr>
                <tr><th>Subject</th><td>#{subject res}</td></tr>
                <tr><th>Message</th><td>#{message res}</td></tr>
                <tr><th>Agree</th><td>#{agree res}</td></tr>
                <tr><th>Question</th><td>#{question res}</td></tr>
        |]
      _ ->
        [whamlet|
          <section .section>
            <div .container>
              <form method=post action=@{HomeR} enctype=#{enctype}>
                ^{form1}

          <section .section>
            <div .container>
              <table .table>
                <th>Name</th><td>
        |]

postHomeR :: Handler Html
postHomeR = getHomeR

main :: IO ()
main = warp 3100 App
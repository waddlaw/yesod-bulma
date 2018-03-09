{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Main (main) where

import           Data.Text               (Text)
import           Yesod
import           Yesod.Form.Bulma
import           Yesod.Form.Bulma.Fields as BF

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
  , message :: Textarea
  }

basicForm :: Html -> MForm Handler (FormResult Basic, Widget)
basicForm = renderBulma BulmaBasicForm $ Basic
  <$> areq BF.textField ("Text input" `withPlaceholder` "Name") Nothing
  <*> areq BF.textField ("bulma" `withPlaceholder` "Username") Nothing
  <*> areq BF.emailField ("Email input" `withPlaceholder` "Email") Nothing
  <*> areq (BF.selectFieldList [("Select dropdown" :: Text, "v1"),("With options", "vv2")]) "Subject" Nothing
  <*> areq BF.textareaField ("Textarea" `withPlaceholder` "Message") Nothing
  <*  bulmaSubmit
        (BulmaSubmit ("保存" :: Text)
                      "btn-default"
                      [("attribute-name", "attribute-value")]
        )

getHomeR :: Handler Html
getHomeR = do
  ((res, form1), enctype) <- runFormPost basicForm

  defaultLayout $ do
    addStylesheetRemote "//cdnjs.cloudflare.com/ajax/libs/bulma/0.6.2/css/bulma.min.css"
    addScriptRemote "//use.fontawesome.com/releases/v5.0.8/js/all.js"
    case res of
      FormSuccess res ->
        [whamlet|
          <section .section .columns>
            <div .container .column .is-6>
              <form method=post action=@{HomeR} enctype=#{enctype}>
                ^{form1}

            <div .container .column .is-6>
              <table .table>
                <tr><th>Name</th><td>#{name res}</td></tr>
                <tr><th>Username</th><td>#{username res}</td></tr>
                <tr><th>Email</th><td>#{email res}</td></tr>
                <tr><th>Subject</th><td>#{subject res}</td></tr>
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
main = warp 3000 App

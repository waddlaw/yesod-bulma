{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Main (main) where

import           Data.Text        (Text)
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
  { getTextField     :: Text
  -- , getPasswordField :: Text
  , getTextareaField :: Textarea
  -- , getHiddenField  :: Text
  , getIntField      :: Int
  -- , getDayField    :: Day
  -- , getTimeFieldTypeTime :: Text
  -- , getTimeFieldTypeText
  -- , getHtmlField
  , getEmailField    :: Text
  -- , getMultiEmailField
  -- , getSearchField
  -- , getUrlField
  -- , getDoubleField
  -- , getBoolField
  , getCheckBoxField :: Bool
  -- , getFileField
  -- , getSelectField ::
  }

basicForm :: Html -> MForm Handler (FormResult Basic, Widget)
basicForm =
  renderBulma BulmaBasicForm
    $   Basic
    <$> areq bulmaTextField "bulmaTextField" Nothing
    <*> areq bulmaTextareaField "bulmaTextareaField" Nothing
    <*> areq bulmaIntField "bulmaIntField" Nothing
    <*> areq bulmaEmailField "bulmaEmailField" Nothing
    <*> areq (bulmaCheckBoxField "bulmaCheckBoxField") "" Nothing
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
    [whamlet| $newline never
      <section .section>
        <div .container>
          <form method=post action=@{HomeR} enctype=#{enctype}>
            ^{form1}
    |]

postHomeR :: Handler Html
postHomeR = getHomeR

main :: IO ()
main = warp 3100 App

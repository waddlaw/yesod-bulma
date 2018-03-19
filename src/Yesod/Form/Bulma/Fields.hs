{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
module Yesod.Form.Bulma.Fields
  ( bulmaTextField
  , bulmaEmailField
  , bulmaTextareaField
  , bulmaCheckBoxField
  , bulmaRadioFieldList
  , bulmaRadioField
  , bulmaSelectFieldList
  , bulmaSelectField
  ) where

import           Control.Monad            (forM_, unless)
import           Data.Maybe               (listToMaybe)
import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Text.Email.Validate      as Email
import           Text.Shakespeare.I18N    (RenderMessage, SomeMessage (..))
import           Yesod.Core               (HandlerSite)
import           Yesod.Core.Types         (HandlerFor, WidgetFor)
import           Yesod.Core.Widget        (handlerToWidget, whamlet)
import           Yesod.Form.Fields        (FormMessage (..), Option (..),
                                           OptionList (..), Textarea (..),
                                           optionsPairs)
import           Yesod.Form.Functions     (parseHelper)
import           Yesod.Form.Types         (Enctype (..), Field (..))

-- | Creates a input with @type="text"@.
bulmaTextField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Text
bulmaTextField = Field
  { fieldParse = parseHelper Right
  , fieldView = \theId name attrs val isReq ->
      [whamlet| $newline never
        <input id="#{theId}" .input name="#{name}" *{attrs} type="text" :isReq:required value="#{either id id val}">
      |]
  , fieldEnctype = UrlEncoded
  }

-- | Creates an input with @type="email"@. Yesod will validate the email's correctness according to RFC5322 and canonicalize it by removing comments and whitespace (see "Text.Email.Validate").
bulmaEmailField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Text
bulmaEmailField = Field
  { fieldParse = parseHelper $
    \s ->
      case Email.canonicalizeEmail $ encodeUtf8 s of
        Just e  -> Right $ decodeUtf8With lenientDecode e
        Nothing -> Left $ MsgInvalidEmail s
  , fieldView = \theId name attrs val isReq ->
      [whamlet| $newline never
        <input id="#{theId}" .input name="#{name}" *{attrs} type="email" :isReq:required="" value="#{either id id val}">
      |]
  , fieldEnctype = UrlEncoded
  }

-- | Creates a @\<textarea>@ tag whose returned value is wrapped in a 'Textarea'; see 'Textarea' for details.
bulmaTextareaField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Textarea
bulmaTextareaField = Field
  { fieldParse = parseHelper $ Right . Textarea
  , fieldView = \theId name attrs val isReq ->
      [whamlet| $newline never
        <textarea id="#{theId}" .textarea name="#{name}" :isReq:required="" *{attrs}>#{either id unTextarea val}
      |]
  , fieldEnctype = UrlEncoded
  }

-- | Creates an input with @type="checkbox"@.
--   While the default @'boolField'@ implements a radio button so you
--   can differentiate between an empty response (@Nothing@) and a no
--   response (@Just False@), this simpler checkbox field returns an empty
--   response as @Just False@.
--
--   Note that this makes the field always optional.
--
bulmaCheckBoxField :: Monad m => Text -> Field m Bool
bulmaCheckBoxField msg = Field
  { fieldParse = \e _ -> return $ checkBoxParser e
  , fieldView  = \theId name attrs val _ ->
      [whamlet| $newline never
        <label .checkbox>
          <input id=#{theId} *{attrs} type=checkbox name=#{name} value=yes :showVal id val:checked> #{msg}
      |]
  , fieldEnctype = UrlEncoded
  }
  where
    checkBoxParser [] = Right $ Just False
    checkBoxParser (x:_) = case x of
        "yes" -> Right $ Just True
        "on"  -> Right $ Just True
        _     -> Right $ Just False
    showVal = either (\_ -> False)

-- | Creates an input with @type="radio"@ for selecting one option.
bulmaRadioFieldList :: (Eq a, RenderMessage site FormMessage, RenderMessage site msg)
               => [(msg, a)]
               -> Field (HandlerFor site) a
bulmaRadioFieldList = bulmaRadioField . optionsPairs

-- | Creates an input with @type="radio"@ for selecting one option.
bulmaRadioField :: (Eq a, RenderMessage site FormMessage)
           => HandlerFor site (OptionList a)
           -> Field (HandlerFor site) a
bulmaRadioField = selectFieldHelper
  (\theId _name _attrs inside ->
    [whamlet| $newline never
      <div ##{theId}>^{inside}
    |])
  (\theId name isSel ->
    [whamlet| $newline never
      <label .radio for=#{theId}-none>
        <div>
          <input id=#{theId}-none type=radio name=#{name} value=none :isSel:checked> _{MsgSelectNone}
    |])
  (\theId name attrs value isSel text ->
    [whamlet| $newline never
      <label .radio for=#{theId}-#{value}>
        <div>
          <input id=#{theId}-#{value} type=radio name=#{name} value=#{value} :isSel:checked *{attrs}> #{text}
    |])

-- | Creates a @\<select>@ tag for selecting one option. Example usage:
--
-- > areq (selectFieldList [("Value 1" :: Text, "value1"),("Value 2", "value2")]) "Which value?" Nothing
bulmaSelectFieldList :: (Eq a, RenderMessage site FormMessage, RenderMessage site msg)
                => [(msg, a)]
                -> Field (HandlerFor site) a
bulmaSelectFieldList = bulmaSelectField . optionsPairs

-- | Creates a @\<select>@ tag for selecting one option. Example usage:
--
-- > areq (selectField $ optionsPairs [(MsgValue1, "value1"),(MsgValue2, "value2")]) "Which value?" Nothing
bulmaSelectField :: (Eq a, RenderMessage site FormMessage)
            => HandlerFor site (OptionList a)
            -> Field (HandlerFor site) a
bulmaSelectField = selectFieldHelper
  (\theId name attrs inside ->
    [whamlet| $newline never
      <div .select>
        <select ##{theId} name=#{name} *{attrs}>
          ^{inside}
    |]) -- outside
  (\_theId _name isSel ->
    [whamlet| $newline never
      <option value=none :isSel:selected>_{MsgSelectNone}
    |]) -- onOpt
  (\_theId _name _attrs value isSel text ->
    [whamlet| $newline never
      <option value=#{value} :isSel:selected>#{text}
    |]) -- inside

-- port from Yesod.Form.Fields
selectFieldHelper
        :: (Eq a, RenderMessage site FormMessage)
        => (Text -> Text -> [(Text, Text)] -> WidgetFor site () -> WidgetFor site ())
        -> (Text -> Text -> Bool -> WidgetFor site ())
        -> (Text -> Text -> [(Text, Text)] -> Text -> Bool -> Text -> WidgetFor site ())
        -> HandlerFor site (OptionList a)
        -> Field (HandlerFor site) a
selectFieldHelper outside onOpt inside opts' = Field
  { fieldParse = \x _ -> do
    opts <- opts'
    return $ selectParser opts x
  , fieldView = \theId name attrs val isReq -> do
    opts <- olOptions <$> handlerToWidget opts'
    outside theId name attrs $ do
      unless isReq $ onOpt theId name $ notElem (render opts val) $ map optionExternalValue opts
      forM_ opts $ \opt -> inside
        theId
        name
        ((if isReq then (("required", "required"):) else id) attrs)
        (optionExternalValue opt)
        (render opts val == optionExternalValue opt)
        (optionDisplay opt)
  , fieldEnctype = UrlEncoded
  }
  where
    render _ (Left _) = ""
    render opts (Right a) = maybe "" optionExternalValue $ listToMaybe $ filter ((== a) . optionInternalValue) opts
    selectParser _ [] = Right Nothing
    selectParser opts (s:_) = case s of
            "" -> Right Nothing
            "none" -> Right Nothing
            x -> case olReadExternal opts x of
                    Nothing -> Left $ SomeMessage $ MsgInvalidEntry x
                    Just y  -> Right $ Just y

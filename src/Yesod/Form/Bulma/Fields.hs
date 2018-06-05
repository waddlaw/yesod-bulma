{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
module Yesod.Form.Bulma.Fields
  ( bulmaIntField
  , bulmaTextField
  , bulmaEmailField
  , bulmaTextareaField
  , bulmaCheckBoxField
  , bulmaRadioFieldList
  , bulmaRadioField
  , bulmaSelectFieldList
  , bulmaSelectField
  , bulmaCheckboxesFieldList
  , bulmaCheckboxesField
  , bulmaMultiSelectFieldList
  , bulmaMultiSelectField
  ) where

import           Control.Arrow            ((&&&))
import           Control.Monad            (forM_, unless)
import           Data.Maybe               (listToMaybe)
import           Data.Text                (Text, pack)
import           Data.Text.Encoding       (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Text.Read           (decimal, signed)
import qualified Text.Email.Validate      as Email
import           Text.Shakespeare.I18N    (RenderMessage, SomeMessage (..))
import           Yesod.Core               (HandlerSite)
import           Yesod.Core.Types         (HandlerFor, WidgetFor)
import           Yesod.Core.Widget        (handlerToWidget, whamlet)
import           Yesod.Form.Bulma.Class
import           Yesod.Form.Bulma.Utils   (addStylesheet')
import           Yesod.Form.Fields        (FormMessage (..), Option (..),
                                           OptionList (..), Textarea (..),
                                           optionsPairs)
import           Yesod.Form.Functions     (parseHelper)
import           Yesod.Form.Types         (Enctype (..), Field (..))

-- | Creates an input with @type="checkbox"@ for selecting multiple options.
bulmaCheckboxesFieldList
  :: ( YesodBulma site
     , Eq a
     , RenderMessage site msg
     )
  => [(msg, a)] -> Field (HandlerFor site) [a]
bulmaCheckboxesFieldList = bulmaCheckboxesField . optionsPairs

-- | Creates an input with @type="checkbox"@ for selecting multiple options.
bulmaCheckboxesField
  :: ( YesodBulma site
     , Eq a
     )
  => HandlerFor site (OptionList a) -> Field (HandlerFor site) [a]
bulmaCheckboxesField ioptlist = (bulmaMultiSelectField ioptlist)
  { fieldView = \theId name attrs val _isReq -> do
      opts <- fmap olOptions $ handlerToWidget ioptlist
      let
        optselected (Left _) _       = False
        optselected (Right vals) opt = (optionInternalValue opt) `elem` vals
      addStylesheet' urlBulmaExCheckRadio
      [whamlet| $newline never
        <div ##{theId}>
          $forall opt <- opts
            <input .is-checkradio type=checkbox id=#{name}-#{optionExternalValue opt} name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected val opt:checked>
            <label for=#{name}-#{optionExternalValue opt}>#{optionDisplay opt}
      |]
  }

-- | Creates a @\<select>@ tag for selecting multiple options.
bulmaMultiSelectFieldList
  :: ( Eq a
     , RenderMessage site msg
     )
  => [(msg, a)] -> Field (HandlerFor site) [a]
bulmaMultiSelectFieldList = bulmaMultiSelectField . optionsPairs

-- | Creates a @\<select>@ tag for selecting multiple options.
bulmaMultiSelectField
  :: Eq a
  => HandlerFor site (OptionList a) -> Field (HandlerFor site) [a]
bulmaMultiSelectField ioptlist = Field parse view UrlEncoded
 where
  parse []      _ = return $ Right Nothing
  parse optlist _ = do
    mapopt <- olReadExternal <$> ioptlist
    case mapM mapopt optlist of
      Nothing  -> return $ Left "Error parsing values"
      Just res -> return $ Right $ Just res

  view theId name attrs val isReq = do
    opts <- fmap olOptions $ handlerToWidget ioptlist
    let selOpts = map (id &&& (optselected val)) opts
    [whamlet| $newline never
      <div .select .is-multiple>
        <select ##{theId} name=#{name} :isReq:required multiple size=#{min 5 (length selOpts)} *{attrs}>
          $forall (opt, optsel) <- selOpts
            <option value=#{optionExternalValue opt} :optsel:selected>#{optionDisplay opt}
    |]
    where
      optselected (Left _) _       = False
      optselected (Right vals) opt = (optionInternalValue opt) `elem` vals

-- | Creates a input with @type="number"@ and @step=1@.
bulmaIntField
  :: ( Monad m
     , Integral i
     , RenderMessage (HandlerSite m) FormMessage
     )
  => Field m i
bulmaIntField = Field
  { fieldParse = parseHelper $ \s ->
    case signed decimal s of
      Right (a, "") -> Right a
      _             -> Left $ MsgInvalidInteger s
  , fieldView = \theId name attrs val isReq ->
      [whamlet| $newline never
        <input id="#{theId}" .input name="#{name}" *{attrs} type="number" step=1 :isReq:required="" value="#{showVal val}">
      |]
  , fieldEnctype = UrlEncoded
  }
  where
    showVal = either id (pack . showI)
    showI x = show (fromIntegral x :: Integer)

-- | Creates a input with @type="text"@.
bulmaTextField
  :: ( Monad m
     , RenderMessage (HandlerSite m) FormMessage
     )
  => Field m Text
bulmaTextField = Field
  { fieldParse = parseHelper Right
  , fieldView = \theId name attrs val isReq ->
      [whamlet| $newline never
        <input id="#{theId}" .input name="#{name}" *{attrs} type="text" :isReq:required value="#{either id id val}">
      |]
  , fieldEnctype = UrlEncoded
  }

-- | Creates an input with @type="email"@. Yesod will validate the email's correctness according to RFC5322 and canonicalize it by removing comments and whitespace (see "Text.Email.Validate").
bulmaEmailField
  :: ( Monad m
     , RenderMessage (HandlerSite m) FormMessage
     )
  => Field m Text
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
bulmaTextareaField
  :: ( Monad m
     , RenderMessage (HandlerSite m) FormMessage
     )
  => Field m Textarea
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
bulmaCheckBoxField
  :: YesodBulma site
  => Text -> Field (HandlerFor site) Bool
bulmaCheckBoxField msg = Field
  { fieldParse = \e _ -> return $ checkBoxParser e
  , fieldView  = \theId name attrs val _ -> do
      addStylesheet' urlBulmaExCheckRadio
      [whamlet| $newline never
        <input .is-checkradio id=#{theId} *{attrs} type=checkbox name=#{name} value=yes :showVal id val:checked>
        <label for=#{theId}>#{msg}
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
bulmaRadioFieldList
  :: ( YesodBulma site
     , Eq a
     , RenderMessage site FormMessage
     , RenderMessage site msg
     )
  => [(msg, a)] -> Field (HandlerFor site) a
bulmaRadioFieldList = bulmaRadioField . optionsPairs

-- | Creates an input with @type="radio"@ for selecting one option.
bulmaRadioField
  :: ( YesodBulma site
     , Eq a
     , RenderMessage site FormMessage
     )
  => HandlerFor site (OptionList a) -> Field (HandlerFor site) a
bulmaRadioField = selectFieldHelper
  (\theId _name _attrs inside -> do
    addStylesheet' urlBulmaExCheckRadio
    [whamlet| $newline never
      <div ##{theId}>^{inside}
    |])
  (\theId name isSel -> do
    addStylesheet' urlBulmaExCheckRadio
    [whamlet| $newline never
      <input .is-checkradio id=#{theId}-none type=radio name=#{name} value=none :isSel:checked>
      <labelfor=#{theId}-none>_{MsgSelectNone}
    |])
  (\theId name attrs value isSel text -> do
    addStylesheet' urlBulmaExCheckRadio
    [whamlet| $newline never
      <input .is-checkradio id=#{theId}-#{value} type=radio name=#{name} value=#{value} :isSel:checked *{attrs}>
      <label for=#{theId}-#{value}>#{text}
    |])

-- | Creates a @\<select>@ tag for selecting one option. Example usage:
--
-- > areq (selectFieldList [("Value 1" :: Text, "value1"),("Value 2", "value2")]) "Which value?" Nothing
bulmaSelectFieldList
  :: ( Eq a
     , RenderMessage site FormMessage
     , RenderMessage site msg
     )
  => [(msg, a)] -> Field (HandlerFor site) a
bulmaSelectFieldList = bulmaSelectField . optionsPairs

-- | Creates a @\<select>@ tag for selecting one option. Example usage:
--
-- > areq (selectField $ optionsPairs [(MsgValue1, "value1"),(MsgValue2, "value2")]) "Which value?" Nothing
bulmaSelectField
  :: (Eq a, RenderMessage site FormMessage)
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
  :: ( Eq a
     , RenderMessage site FormMessage
     )
  => (Text -> Text -> [(Text, Text)] -> WidgetFor site () -> WidgetFor site ())
  -> (Text -> Text -> Bool -> WidgetFor site ())
  -> (Text -> Text -> [(Text, Text)] -> Text -> Bool -> Text -> WidgetFor site ())
  -> HandlerFor site (OptionList a)
  -> Field (HandlerFor site) a
selectFieldHelper outside onOpt inside opts' = Field
  { fieldParse   = \x _ -> do
    opts <- opts'
    return $ selectParser opts x
  , fieldView    = \theId name attrs val isReq -> do
    opts <- olOptions <$> handlerToWidget opts'
    outside theId name attrs $ do
      unless isReq $ onOpt theId name $ notElem (render opts val) $ map
        optionExternalValue
        opts
      forM_ opts $ \opt -> inside
        theId
        name
        ((if isReq then (("required", "required") :) else id) attrs)
        (optionExternalValue opt)
        (render opts val == optionExternalValue opt)
        (optionDisplay opt)
  , fieldEnctype = UrlEncoded
  }
  where
    render _    (Left  _) = ""
    render opts (Right a) = maybe "" optionExternalValue $ listToMaybe $ filter
      ((== a) . optionInternalValue)
      opts
    selectParser _    []      = Right Nothing
    selectParser opts (s : _) = case s of
      ""     -> Right Nothing
      "none" -> Right Nothing
      x      -> case olReadExternal opts x of
        Nothing -> Left $ SomeMessage $ MsgInvalidEntry x
        Just y  -> Right $ Just y

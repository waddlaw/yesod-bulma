{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
module Yesod.Form.Bulma.Fields where

import           Control.Monad         (unless)
import           Data.Maybe            (listToMaybe)
import           Data.Text             (Text)
import           Text.Shakespeare.I18N (RenderMessage, SomeMessage (..))
import           Yesod.Core.Types      (HandlerT, WidgetT)
import           Yesod.Core.Widget     (handlerToWidget, whamlet)
import           Yesod.Form.Fields     (FormMessage (..), Option (..),
                                        OptionList (..), optionsPairs)
import           Yesod.Form.Types      (Enctype (..), Field (..))

-- | Creates a @\<select>@ tag for selecting one option. Example usage:
--
-- > areq (selectFieldList [("Value 1" :: Text, "value1"),("Value 2", "value2")]) "Which value?" Nothing
selectFieldList :: (Eq a, RenderMessage site FormMessage, RenderMessage site msg)
                => [(msg, a)]
                -> Field (HandlerT site IO) a
selectFieldList = selectField . optionsPairs

-- | Creates a @\<select>@ tag for selecting one option. Example usage:
--
-- > areq (selectField $ optionsPairs [(MsgValue1, "value1"),(MsgValue2, "value2")]) "Which value?" Nothing
selectField :: (Eq a, RenderMessage site FormMessage)
            => HandlerT site IO (OptionList a)
            -> Field (HandlerT site IO) a
selectField = selectFieldHelper
    (\theId name attrs inside ->
      [whamlet| $newline never
        <div .select ##{theId} name=#{name} *{attrs}>
          <select>
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
        => (Text -> Text -> [(Text, Text)] -> WidgetT site IO () -> WidgetT site IO ())
        -> (Text -> Text -> Bool -> WidgetT site IO ())
        -> (Text -> Text -> [(Text, Text)] -> Text -> Bool -> Text -> WidgetT site IO ())
        -> HandlerT site IO (OptionList a)
        -> Field (HandlerT site IO) a
selectFieldHelper outside onOpt inside opts' = Field
    { fieldParse = \x _ -> do
        opts <- opts'
        return $ selectParser opts x
    , fieldView = \theId name attrs val isReq -> do
        opts <- fmap olOptions $ handlerToWidget opts'
        outside theId name attrs $ do
            unless isReq $ onOpt theId name $ not $ render opts val `elem` map optionExternalValue opts
            flip mapM_ opts $ \opt -> inside
                theId
                name
                ((if isReq then (("required", "required"):) else id) attrs)
                (optionExternalValue opt)
                ((render opts val) == optionExternalValue opt)
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

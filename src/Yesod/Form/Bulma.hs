{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
module Yesod.Form.Bulma (renderBulma, bfs, bulmaSubmit, BulmaSubmit(..), BulmaFormLayout(..), withPlaceholder) where

import           Data.Bifunctor        (second)
import           Data.Text             (Text)
import           Text.Julius           (julius, rawJS)
import           Text.Shakespeare.I18N (RenderMessage, SomeMessage (..))
import           Yesod.Core            (HandlerSite, MonadHandler)
import           Yesod.Core.Handler    (newIdent)
import           Yesod.Core.Types      (WidgetT)
import           Yesod.Core.Widget     (toWidget, whamlet)
import           Yesod.Form.Functions  (FormRender, aFormToForm, formToAForm)
import           Yesod.Form.Types      (AForm, FieldSettings (..),
                                        FieldView (..), FormResult (..), MForm)

data BulmaFormLayout = BulmaBasicForm

data BulmaSubmit msg =
  BulmaSubmit
    { _bulmaValue   :: msg -- ^ The text of the submit button.
    , _bulmaClasses :: Text -- ^ Classes added to the @\<button>@.
    , _bulmaAttrs   :: [(Text, Text)] -- ^ Attributes added to the @\<button>@.
    } deriving Show

renderBulma :: Monad m => BulmaFormLayout -> FormRender m a
renderBulma formLayout aform fragment = do
  (res, views') <- aFormToForm aform
  let
    views = views' []
    widget = do
      cancelId <- newIdent
      [whamlet| $#newline never
        #{fragment}
        $forall view <- views
          $if fvId view == bulmaSubmitId
            <div .field .is-grouped>
              <div .control>
                <button .button .is-link>Submit
          $else
            <div .field :fvRequired view:.required :not $ fvRequired view:.optional :has $ fvErrors view:.is-danger>
              $case formLayout
                $of BulmaBasicForm
                  <label .label for=#{fvId view}>#{fvLabel view}
                  <div .control>
                    ^{fvInput view}
                    ^{helpWidget view}
      |]
  return (res, widget)
  where
    has (Just _) = True
    has Nothing  = False

bulmaSubmit :: (RenderMessage site msg, HandlerSite m ~ site, MonadHandler m) => BulmaSubmit msg -> AForm m ()
bulmaSubmit= formToAForm . fmap (second return) . mbulmaSubmit

mbulmaSubmit
    :: (RenderMessage site msg, HandlerSite m ~ site, MonadHandler m)
    => BulmaSubmit msg -> MForm m (FormResult (), FieldView site)
mbulmaSubmit (BulmaSubmit msg classes attrs) =
    let res = FormSuccess ()
        widget = [whamlet|
            $newline never
            <button class="btn #{classes}" type=submit *{attrs}>_{msg}
          |]
        fv  = FieldView { fvLabel    = ""
                        , fvTooltip  = Nothing
                        , fvId       = bulmaSubmitId
                        , fvInput    = widget
                        , fvErrors   = Nothing
                        , fvRequired = False }
    in return (res, fv)

-- | (Internal) Render a help widget for tooltips and errors.
helpWidget :: FieldView site -> WidgetT site IO ()
helpWidget view = [whamlet|
    $maybe tt <- fvTooltip view
      <span .help-block>#{tt}
    $maybe err <- fvErrors view
      <span .help-block .error-block>#{err}
|]

bulmaSubmitId :: Text
bulmaSubmitId = "b:ulma___unique__:::::::::::::::::submit-id"

bfs :: RenderMessage site Text => Text -> FieldSettings site
bfs msg = FieldSettings (SomeMessage msg) Nothing Nothing Nothing [("class", "input")]

withPlaceholder :: Text -> FieldSettings site -> FieldSettings site
withPlaceholder placeholder fs = fs { fsAttrs = newAttrs }
  where newAttrs = ("placeholder", placeholder) : fsAttrs fs
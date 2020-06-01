module Components.BoundsInput where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (XYBounds)

type BoundsInputSlot p
  = forall q. H.Slot q BoundsInputMessage p

type State
  = { error :: Maybe String
    , xBounds ::
        { upper :: String
        , lower :: String
        }
    , yBounds ::
        { upper :: String
        , lower :: String
        }
    }

data Bound
  = XLower
  | XUpper
  | YLower
  | YUpper

data BoundsInputMessage
  = Updated XYBounds

data Action
  = Init
  | Recieve XYBounds
  | HandleInput Bound String
  | Update

boundsInputComponent :: forall query m. MonadEffect m => H.Component HH.HTML query XYBounds BoundsInputMessage m
boundsInputComponent =
  H.mkComponent
    { initialState: initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , receive = Just <<< Recieve
              , initialize = Just Init
              }
    }

initialState :: XYBounds -> State
initialState input =
  { error: Nothing
  , xBounds:
      { upper: show input.xBounds.upper
      , lower: show input.xBounds.lower
      }
  , yBounds:
      { upper: show input.yBounds.upper
      , lower: show input.yBounds.lower
      }
  }

render :: forall slots m. State -> HH.ComponentHTML Action slots m
render state =
  HH.div_
    [ HH.label
        [ HP.for "xLower" ]
        [ HH.text "Lower X:" ]
    , HH.input
        [ HP.type_ HP.InputText
        , HE.onValueChange $ toValueChangeActionEvent XLower
        , HP.value state.xBounds.lower
        , HP.id_ "xLower"
        ]
    , HH.br_
    , HH.label
        [ HP.for "xUpper" ]
        [ HH.text "Upper X:" ]
    , HH.input
        [ HP.type_ HP.InputText
        , HE.onValueChange $ toValueChangeActionEvent XUpper
        , HP.value state.xBounds.upper
        , HP.id_ "xUpper"
        ]
    , HH.br_
    , HH.label
        [ HP.for "yLower" ]
        [ HH.text "Lower Y:" ]
    , HH.input
        [ HP.type_ HP.InputText
        , HE.onValueChange $ toValueChangeActionEvent YLower
        , HP.value state.yBounds.lower
        , HP.id_ "yLower"
        ]
    , HH.br_
    , HH.label
        [ HP.for "yUpper" ]
        [ HH.text "Upper Y:" ]
    , HH.input
        [ HP.type_ HP.InputText
        , HE.onValueChange $ toValueChangeActionEvent YUpper
        , HP.value state.yBounds.upper
        , HP.id_ "yUpper"
        ]
    , HH.br_
    , HH.button
        [ HE.onClick $ toActionEvent Update ]
        [ HH.text "Update" ]
    , HH.p_
        [ HH.text $ fromMaybe "" state.error ]
    ]

toValueChangeActionEvent :: Bound -> String -> Maybe Action
toValueChangeActionEvent bound value = Just $ HandleInput bound value

toActionEvent :: forall a. Action -> a -> Maybe Action
toActionEvent action _ = Just action

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () BoundsInputMessage m Unit
handleAction = case _ of
  Init -> do
    { xBounds, yBounds } <- H.get
    handleAction (HandleInput XLower $ xBounds.lower)
    handleAction (HandleInput XUpper $ xBounds.upper)
    handleAction (HandleInput YLower $ yBounds.lower)
    handleAction (HandleInput YUpper $ yBounds.upper)
  HandleInput XLower stringInput -> H.modify_ _ { xBounds { lower = stringInput } }
  HandleInput XUpper stringInput -> H.modify_ _ { xBounds { upper = stringInput } }
  HandleInput YLower stringInput -> H.modify_ _ { yBounds { lower = stringInput } }
  HandleInput YUpper stringInput -> H.modify_ _ { yBounds { upper = stringInput } }
  Recieve bounds ->
    H.modify_
      _
        { xBounds
          { upper = show bounds.xBounds.upper
          , lower = show bounds.xBounds.lower
          }
        , yBounds
          { upper = show bounds.yBounds.upper
          , lower = show bounds.yBounds.lower
          }
        }
  Update -> do
    { xBounds, yBounds } <- H.get
    let
      maybeXLower = fromString xBounds.lower

      maybeXUpper = fromString xBounds.upper

      maybeYLower = fromString yBounds.lower

      maybeYUpper = fromString yBounds.upper
    case maybeXLower of
      Nothing -> H.modify_ _ { error = Just $ "Failed to parse lower X bound" }
      Just xLower -> case maybeXUpper of
        Nothing -> H.modify_ _ { error = Just $ "Failed to parse upper X bound" }
        Just xUpper -> case maybeYLower of
          Nothing -> H.modify_ _ { error = Just $ "Failed to parse lower Y bound" }
          Just yLower -> case maybeYUpper of
            Nothing -> H.modify_ _ { error = Just $ "Failed to parse upper Y bound" }
            Just yUpper -> H.raise (Updated { xBounds: { lower: xLower, upper: xUpper }, yBounds: { lower: yLower, upper: yUpper } })

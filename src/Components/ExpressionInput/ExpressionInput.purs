module Components.ExpressionInput where

import Prelude
import Components.ExpressionInput.Controller (ExpressionInputController)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Expression.Syntax (Expression)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..))
import Halogen.HTML.Properties as HP

type ExpressionInputSlot p
  = forall q. H.Slot q ExpressionInputMessage p

type State
  = { input :: String
    , error :: Maybe String
    , id :: Int
    , status :: Status
    }

data ExpressionInputMessage
  = Parsed Int Expression String
  | ChangedStatus Int Status

data Action
  = Init
  | HandleInput String
  | HandleMessage (Tuple String Status)
  | Parse
  | Status Status

data Status
  = Off
  | Rough
  | Robust

derive instance statusEq :: Eq Status

expressionInputComponent :: forall query m. MonadEffect m => ExpressionInputController -> Int -> H.Component HH.HTML query (Tuple String Status) ExpressionInputMessage m
expressionInputComponent controller id =
  H.mkComponent
    { initialState: initialState id
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction controller
              , receive = Just <<< HandleMessage
              , initialize = Just Init
              }
    }

initialState :: Int -> Tuple String Status -> State
initialState id (Tuple input status) =
  { input
  , error: Nothing
  , id
  , status
  }

render :: forall slots m. State -> HH.ComponentHTML Action slots m
render state =
  HH.div_
    $ [ HH.form_
          [ HH.div
              [ HP.class_ (ClassName "input-group mb-3") ]
              [ HH.div
                  [ HP.class_ (ClassName "input-group-prepend") ]
                  [ HH.span [ HP.class_ (ClassName "input-group-text") ] [ HH.text "f(x)=" ] ]
              , HH.input
                  [ HP.type_ HP.InputText
                  , HE.onValueChange $ toValueChangeActionEvent
                  , HP.value state.input
                  , HP.class_ (ClassName "form-control")
                  ]
              , HH.button
                  [ HP.type_ ButtonButton
                  , HP.class_ (ClassName "btn btn-primary")
                  , HE.onClick $ toActionEvent Parse
                  ]
                  [ HH.text "Plot" ]
              ]
          , HH.div
              [ HP.class_ (ClassName "form-check form-check-inline") ]
              [ HH.input
                  [ HP.type_ HP.InputRadio
                  , HE.onChecked $ toCheckedEvent (Status Off)
                  , HP.id_ "offCheckBox"
                  , HP.checked (state.status == Off)
                  , HP.class_ (ClassName "form-check-input")
                  ]
              , HH.label
                  [ HP.for "offCheckBox", HP.class_ (ClassName "form-check-label pr-3") ]
                  [ HH.text "Off" ]
              , HH.input
                  [ HP.type_ HP.InputRadio
                  , HE.onChecked $ toCheckedEvent (Status Rough)
                  , HP.id_ "roughCheckBox"
                  , HP.checked (state.status == Rough)
                  , HP.class_ (ClassName "form-check-input")
                  ]
              , HH.label
                  [ HP.for "roughCheckBox", HP.class_ (ClassName "form-check-label pr-3") ]
                  [ HH.text "Rough" ]
              , HH.input
                  [ HP.type_ HP.InputRadio
                  , HE.onChecked $ toCheckedEvent (Status Robust)
                  , HP.id_ "robustCheckBox"
                  , HP.checked (state.status == Robust)
                  , HP.class_ (ClassName "form-check-input")
                  ]
              , HH.label
                  [ HP.for "robustCheckBox", HP.class_ (ClassName "form-check-label") ]
                  [ HH.text "Rough and Robust" ]
              ]
          ]
      ]
    <> (errorMessage state.error)

toValueChangeActionEvent :: String -> Maybe Action
toValueChangeActionEvent value = Just $ HandleInput value

toActionEvent :: forall a. Action -> a -> Maybe Action
toActionEvent action _ = Just action

toCheckedEvent :: Action -> Boolean -> Maybe Action
toCheckedEvent action true = Just action

toCheckedEvent _ false = Nothing

errorMessage :: forall slots m. Maybe String -> Array (HH.ComponentHTML Action slots m)
errorMessage Nothing = []

errorMessage (Just message) =
  [ HH.div
      [ HP.class_ (ClassName "alert alert-danger") ]
      [ HH.text message ]
  ]

handleAction :: forall m. MonadEffect m => ExpressionInputController -> Action -> H.HalogenM State Action () ExpressionInputMessage m Unit
handleAction controller = case _ of
  Init -> do
    { input } <- H.get
    handleAction controller (HandleInput input)
    pure unit
  Parse -> do
    { input, id } <- H.get
    let
      result = controller.parse input
    case result of
      Left parseError -> H.modify_ _ { error = Just $ show parseError }
      Right expression -> case controller.check expression of
        Left evaluationError -> H.modify_ _ { error = Just $ show evaluationError }
        Right _ -> do
          H.modify_ _ { error = Nothing }
          H.raise (Parsed id (controller.clean expression) input)
  HandleInput input -> H.modify_ _ { input = input }
  HandleMessage (Tuple input status) -> H.modify_ _ { input = input, status = status }
  Status status -> do
    { id } <- H.get
    H.modify_ _ { status = status }
    H.raise (ChangedStatus id status)

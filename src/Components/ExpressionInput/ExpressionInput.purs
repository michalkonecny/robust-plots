module Components.ExpressionInput where

import Prelude
import Components.ExpressionInput.Controller (ExpressionInputController)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Effect.Class (class MonadEffect)
import Expression.Syntax (Expression)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type ExpressionInputSlot p
  = forall q. H.Slot q ExpressionInputMessage p

type State
  = { expressionInput :: String
    , accuracyInput :: String
    , error :: Maybe String
    , id :: Int
    , input :: Input
    }

type Input
  = { expressionText :: String
    , status :: Status
    , accuracy :: Number
    }

data ExpressionInputMessage
  = ParsedExpression Int Expression String
  | ChangedStatus Int Status
  | ParsedAccuracy Int Number

data Action
  = HandleExpressionInput String
  | HandleMessage Input
  | UpdateExpression
  | UpdateAccuracy
  | Status Status
  | HandleAccuracyInput String

data Status
  = Off
  | Rough
  | Robust

derive instance statusEq :: Eq Status

expressionInputComponent :: forall query m. MonadEffect m => ExpressionInputController -> Int -> H.Component HH.HTML query Input ExpressionInputMessage m
expressionInputComponent controller id =
  H.mkComponent
    { initialState: initialState id
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction controller
              , receive = Just <<< HandleMessage
              }
    }

initialState :: Int -> Input -> State
initialState id input =
  { input
  , error: Nothing
  , id
  , expressionInput: input.expressionText
  , accuracyInput: show input.accuracy
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
                  , HE.onValueChange $ toValueChangeActionEvent HandleExpressionInput
                  , HP.value state.expressionInput
                  , HE.onFocusOut $ toActionEvent UpdateExpression
                  , HP.class_ (ClassName "form-control")
                  ]
              ]
          , HH.div
              [ HP.class_ (ClassName "form-check form-check-inline") ]
              [ HH.input
                  [ HP.type_ HP.InputRadio
                  , HE.onChecked $ toCheckedEvent (Status Off)
                  , HP.id_ "offCheckBox"
                  , HP.checked (state.input.status == Off)
                  , HP.class_ (ClassName "form-check-input")
                  ]
              , HH.label
                  [ HP.for "offCheckBox", HP.class_ (ClassName "form-check-label pr-3") ]
                  [ HH.text "Off" ]
              , HH.input
                  [ HP.type_ HP.InputRadio
                  , HE.onChecked $ toCheckedEvent (Status Rough)
                  , HP.id_ "roughCheckBox"
                  , HP.checked (state.input.status == Rough)
                  , HP.class_ (ClassName "form-check-input")
                  ]
              , HH.label
                  [ HP.for "roughCheckBox", HP.class_ (ClassName "form-check-label pr-3") ]
                  [ HH.text "Rough" ]
              , HH.input
                  [ HP.type_ HP.InputRadio
                  , HE.onChecked $ toCheckedEvent (Status Robust)
                  , HP.id_ "robustCheckBox"
                  , HP.checked (state.input.status == Robust)
                  , HP.class_ (ClassName "form-check-input")
                  ]
              , HH.label
                  [ HP.for "robustCheckBox", HP.class_ (ClassName "form-check-label pr-2") ]
                  [ HH.text "Robust with accuracy" ]
              , HH.input
                  [ HP.type_ HP.InputText
                  , HE.onValueChange $ toValueChangeActionEvent HandleAccuracyInput
                  , HP.value state.accuracyInput
                  , HE.onFocusOut $ toActionEvent UpdateAccuracy
                  , HP.class_ (ClassName "form-control small-input")
                  ]
              , HH.span [ HP.class_ (ClassName "form-check-label pl-2") ] [ HH.text "px" ]
              ]
          ]
      ]
    <> (errorMessage state.error)

toValueChangeActionEvent :: (String -> Action) -> String -> Maybe Action
toValueChangeActionEvent action value = Just $ action value

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
  UpdateExpression -> do
    { expressionInput, id } <- H.get
    case controller.parse expressionInput of
      Left parseError -> H.modify_ _ { error = Just $ show parseError }
      Right expression -> case controller.check expression of
        Left evaluationError -> H.modify_ _ { error = Just $ show evaluationError }
        Right _ -> do
          H.modify_ _ { error = Nothing }
          H.raise (ParsedExpression id (controller.clean expression) expressionInput)
  UpdateAccuracy -> do
    { accuracyInput, id } <- H.get
    case checkValid accuracyInput of
      Right error -> H.modify_ _ { error = Just error }
      Left accuracy -> do
        H.modify_ _ { error = Nothing }
        H.raise (ParsedAccuracy id accuracy)
  HandleExpressionInput input -> H.modify_ _ { expressionInput = input }
  HandleAccuracyInput input -> H.modify_ _ { accuracyInput = input }
  HandleMessage input -> H.modify_ _ { input = input, expressionInput = input.expressionText, accuracyInput = show input.accuracy }
  Status status -> do
    { id } <- H.get
    H.raise (ChangedStatus id status)

checkValid :: String -> Either Number String
checkValid accuracyString = case fromString accuracyString of
  Nothing -> Right "Failed to parse Accuracy"
  Just accuracy -> Left accuracy

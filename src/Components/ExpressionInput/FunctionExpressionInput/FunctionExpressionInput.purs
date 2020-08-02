module Components.ExpressionInput.FunctionExpressionInput where

import Prelude
import Components.Common.Action (onCheckedActionEvent, onEnterPressActionEvent, onFocusOutActionEvent, onValueChangeActionEvent)
import Components.Common.ClassName (className)
import Components.ExpressionInput.Controller (ExpressionInputController)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Expression.Error (Expect)
import Expression.Syntax (Expression)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Types (Id)
import ViewModels.Expression.Common (Status(..))

type FunctionExpressionInputSlot p
  = forall q. H.Slot q FunctionExpressionInputMessage p

type State
  = { expressionInput :: String
    , accuracyInput :: String
    , error :: Maybe String
    , id :: Id
    , input :: Input
    }

type Input
  = { expressionText :: String
    , status :: Status
    , accuracy :: Number
    }

data FunctionExpressionInputMessage
  = FunctionParsedExpression Id Expression String
  | FunctionChangedStatus Id Status
  | FunctionParsedAccuracy Id Number

data Action
  = HandleExpressionInput String
  | HandleMessage Input
  | UpdateExpression
  | UpdateAccuracy
  | Status Status
  | HandleAccuracyInput String

functionExpressionInputComponent :: forall query m. MonadEffect m => ExpressionInputController -> Id -> H.Component HH.HTML query Input FunctionExpressionInputMessage m
functionExpressionInputComponent controller id =
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
              [ className "input-group mb-3" ]
              [ HH.div
                  [ className "input-group-prepend" ]
                  [ HH.span
                      [ className "input-group-text" ]
                      [ HH.text "f(x)=" ]
                  ]
              , HH.input
                  [ HP.type_ HP.InputText
                  , onValueChangeActionEvent HandleExpressionInput
                  , HP.value state.expressionInput
                  , onFocusOutActionEvent UpdateExpression
                  , onEnterPressActionEvent UpdateExpression
                  , className "form-control"
                  ]
              , HH.a
                  [ className "btn btn-info"
                  , HP.href "https://github.com/michalkonecny/robust-plots/blob/master/docs/syntax.md"
                  , HP.target "_blank"
                  ]
                  [ HH.text "🛈" ]
              ]
          , HH.div
              [ className "form-check form-check-inline" ]
              [ HH.input
                  [ HP.type_ HP.InputRadio
                  , onCheckedActionEvent $ Status Off
                  , HP.id_ "offCheckBox"
                  , HP.checked (state.input.status == Off)
                  , className "form-check-input"
                  ]
              , HH.label
                  [ HP.for "offCheckBox"
                  , className "form-check-label pr-3"
                  ]
                  [ HH.text "Off" ]
              , HH.input
                  [ HP.type_ HP.InputRadio
                  , onCheckedActionEvent $ Status Rough
                  , HP.id_ "roughCheckBox"
                  , HP.checked (state.input.status == Rough)
                  , className "form-check-input"
                  ]
              , HH.label
                  [ HP.for "roughCheckBox"
                  , className "form-check-label pr-3"
                  ]
                  [ HH.text "Rough" ]
              , HH.input
                  [ HP.type_ HP.InputRadio
                  , onCheckedActionEvent $ Status Robust
                  , HP.id_ "robustCheckBox"
                  , HP.checked (state.input.status == Robust)
                  , className "form-check-input"
                  ]
              , HH.label
                  [ HP.for "robustCheckBox"
                  , className "form-check-label pr-2"
                  ]
                  [ HH.text "Robust with accuracy" ]
              , HH.input
                  [ HP.type_ HP.InputText
                  , onValueChangeActionEvent HandleAccuracyInput
                  , HP.value state.accuracyInput
                  , onFocusOutActionEvent UpdateAccuracy
                  , onEnterPressActionEvent UpdateAccuracy
                  , className "form-control small-input"
                  ]
              , HH.span
                  [ className "form-check-label pl-2" ]
                  [ HH.text "px" ]
              ]
          ]
      ]
    <> (errorMessage state.error)

errorMessage :: forall slots m. Maybe String -> Array (HH.ComponentHTML Action slots m)
errorMessage Nothing = []

errorMessage (Just message) =
  [ HH.div
      [ className "alert alert-danger" ]
      [ HH.text message ]
  ]

handleAction :: forall m. MonadEffect m => ExpressionInputController -> Action -> H.HalogenM State Action () FunctionExpressionInputMessage m Unit
handleAction controller = case _ of
  UpdateExpression -> do
    { expressionInput, id, input } <- H.get
    case parseAndCheckExpression controller expressionInput of
      Left error -> H.modify_ _ { error = Just $ show error }
      Right expression -> do
        H.modify_ _ { error = Nothing }
        when (expressionInput /= input.expressionText) do
          H.raise (FunctionParsedExpression id (controller.clean expression) expressionInput)
  UpdateAccuracy -> do
    { accuracyInput, id, input } <- H.get
    case controller.checkAccuracy accuracyInput of
      Right error -> H.modify_ _ { error = Just error }
      Left accuracy -> do
        H.modify_ _ { error = Nothing }
        when (accuracyInput /= show input.accuracy) do
          H.raise (FunctionParsedAccuracy id accuracy)
  HandleExpressionInput input -> H.modify_ _ { expressionInput = input }
  HandleAccuracyInput input -> H.modify_ _ { accuracyInput = input }
  HandleMessage input -> H.modify_ _ { input = input, expressionInput = input.expressionText, accuracyInput = show input.accuracy, error = Nothing }
  Status status -> do
    { id } <- H.get
    H.raise (FunctionChangedStatus id status)

parseAndCheckExpression :: ExpressionInputController -> String -> Expect Expression
parseAndCheckExpression controller expressionInput = case controller.parse expressionInput of
  Left parseError -> Left parseError
  Right expression -> case controller.checkExpression "x" expression of
    Left evaluationError -> Left evaluationError
    Right _ -> Right expression

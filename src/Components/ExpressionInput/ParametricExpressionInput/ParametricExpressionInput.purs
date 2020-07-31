module Components.ExpressionInput.ParametricExpressionInput where

import Prelude
import Components.Common.Action (onCheckedActionEvent, onEnterPressActionEvent, onFocusOutActionEvent, onValueChangeActionEvent)
import Components.Common.ClassName (className)
import Components.ExpressionInput.Controller (ExpressionInputController)
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Expression.Error (Expect)
import Expression.Parser (P, literal)
import Expression.Syntax (Expression)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import IntervalArith.Misc (Rational, rationalToNumber)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.Expr (buildExprParser)
import Types (Bounds, Id)
import ViewModels.Expression.Common (Status(..))
import ViewModels.Expression.Parametric (ParametricExpressionText, ParametricExpression)

type HalogenParametricInput m
  = H.HalogenM State Action () ParametricExpressionInputMessage m Unit

type ParametricExpressionInputSlot p
  = forall q. H.Slot q ParametricExpressionInputMessage p

type State
  = { xExpressionInput :: String
    , yExpressionInput :: String
    , domain ::
        { upperInput :: String
        , lowerInput :: String
        }
    , accuracyInput :: String
    , error :: Maybe String
    , id :: Id
    , input :: Input
    }

type Input
  = { expressionText :: ParametricExpressionText
    , status :: Status
    , accuracy :: Number
    , domain :: Bounds
    }

data ParametricExpressionInputMessage
  = ParametricParsedExpression Id ParametricExpression ParametricExpressionText
  | ParametricChangedStatus Id Status
  | ParametricParsedAccuracy Id Number
  | ParametricParsedDomain Id Bounds

data Action
  = HandleXExpressionInput String
  | HandleYExpressionInput String
  | HandleMessage Input
  | UpdateExpression
  | UpdateAccuracy
  | UpdateDomain
  | HandleUpperInput String
  | HandleLowerInput String
  | Status Status
  | HandleAccuracyInput String

parametricExpressionInputComponent :: forall query m. MonadEffect m => ExpressionInputController -> Id -> H.Component HH.HTML query Input ParametricExpressionInputMessage m
parametricExpressionInputComponent controller id =
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

initialState :: Id -> Input -> State
initialState id input =
  { input
  , error: Nothing
  , id
  , xExpressionInput: input.expressionText.xText
  , yExpressionInput: input.expressionText.yText
  , accuracyInput: show input.accuracy
  , domain:
      { upperInput: show $ rationalToNumber input.domain.upper
      , lowerInput: show $ rationalToNumber input.domain.lower
      }
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
                      [ HH.text "x(t)=" ]
                  ]
              , HH.input
                  [ HP.type_ HP.InputText
                  , onValueChangeActionEvent HandleXExpressionInput
                  , HP.value state.xExpressionInput
                  , onFocusOutActionEvent UpdateExpression
                  , onEnterPressActionEvent UpdateExpression
                  , className "form-control"
                  ]
              , HH.a
                  [ className "btn btn-info"
                  , HP.href "https://github.com/michalkonecny/robust-plots/blob/master/docs/syntax.md"
                  ]
                  [ HH.text "🛈" ]
              ]
          , HH.div
              [ className "input-group mb-3" ]
              [ HH.div
                  [ className "input-group-prepend" ]
                  [ HH.span
                      [ className "input-group-text" ]
                      [ HH.text "y(t)=" ]
                  ]
              , HH.input
                  [ HP.type_ HP.InputText
                  , onValueChangeActionEvent HandleYExpressionInput
                  , HP.value state.yExpressionInput
                  , onFocusOutActionEvent UpdateExpression
                  , onEnterPressActionEvent UpdateExpression
                  , className "form-control"
                  ]
              , HH.a
                  [ className "btn btn-info"
                  , HP.href "https://github.com/michalkonecny/robust-plots/blob/master/docs/syntax.md"
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
          , HH.div
              [ className "form-inline" ]
              [ HH.input
                  [ HP.type_ HP.InputText
                  , onValueChangeActionEvent $ HandleLowerInput
                  , HP.value state.domain.lowerInput
                  , onFocusOutActionEvent UpdateDomain
                  , onEnterPressActionEvent UpdateDomain
                  , className "form-control small-input"
                  ]
              , HH.span [] [ HH.text " ≤ t ≤ " ]
              , HH.input
                  [ HP.type_ HP.InputText
                  , onValueChangeActionEvent $ HandleUpperInput
                  , onFocusOutActionEvent UpdateDomain
                  , onEnterPressActionEvent UpdateDomain
                  , HP.value state.domain.upperInput
                  , className "form-control small-input"
                  ]
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

handleAction :: forall m. MonadEffect m => ExpressionInputController -> Action -> H.HalogenM State Action () ParametricExpressionInputMessage m Unit
handleAction controller = case _ of
  UpdateExpression -> do
    { xExpressionInput, yExpressionInput, id, input } <- H.get
    handleError (parseAndCheckExpression controller xExpressionInput)
      $ \xExpression -> do
          handleError (parseAndCheckExpression controller yExpressionInput)
            $ \yExpression -> do
                H.modify_ _ { error = Nothing }
                when (xExpressionInput /= input.expressionText.xText || yExpressionInput /= input.expressionText.yText) do
                  let
                    exprerssion =
                      { xExpression: controller.clean xExpression
                      , yExpression: controller.clean yExpression
                      }

                    text =
                      { xText: xExpressionInput
                      , yText: yExpressionInput
                      }
                  H.raise (ParametricParsedExpression id exprerssion text)
  UpdateAccuracy -> do
    { accuracyInput, id, input } <- H.get
    case controller.checkAccuracy accuracyInput of
      Right error -> H.modify_ _ { error = Just error }
      Left accuracy -> do
        H.modify_ _ { error = Nothing }
        when (accuracyInput /= show input.accuracy) do
          H.raise (ParametricParsedAccuracy id accuracy)
  UpdateDomain -> do
    { domain, id, input } <- H.get
    case parse domain.lowerInput, parse domain.upperInput of
      Just lower, Just upper -> do
        H.modify_ _ { error = Nothing }
        when (lower /= input.domain.lower || upper /= input.domain.upper) do
          H.raise (ParametricParsedDomain id { lower, upper })
      _, _ -> H.modify_ _ { error = Just "Invalid domain" }
  HandleXExpressionInput input -> H.modify_ _ { xExpressionInput = input }
  HandleYExpressionInput input -> H.modify_ _ { yExpressionInput = input }
  HandleAccuracyInput input -> H.modify_ _ { accuracyInput = input }
  HandleUpperInput input -> H.modify_ _ { domain { upperInput = input } }
  HandleLowerInput input -> H.modify_ _ { domain { lowerInput = input } }
  HandleMessage input ->
    H.modify_
      _
        { input = input
        , xExpressionInput = input.expressionText.xText
        , yExpressionInput = input.expressionText.yText
        , accuracyInput = show input.accuracy
        , error = Nothing
        }
  Status status -> do
    { id } <- H.get
    H.raise (ParametricChangedStatus id status)

parseAndCheckExpression :: ExpressionInputController -> String -> Expect Expression
parseAndCheckExpression controller xExpressionInput = case controller.parse xExpressionInput of
  Left parseError -> Left parseError
  Right expression -> case controller.checkExpression "t" expression of
    Left evaluationError -> Left evaluationError
    Right _ -> Right expression

handleError :: forall m. Expect Expression -> (Expression -> HalogenParametricInput m) -> HalogenParametricInput m
handleError (Left error) _ = H.modify_ _ { error = Just $ show error }

handleError (Right expression) onSuccess = onSuccess expression

parse :: String -> Maybe Rational
parse input = case runParser input expressionParser of
  Left error -> Nothing
  Right number -> Just number

expressionParser :: P Rational
expressionParser = fix (\p -> buildExprParser [] literal)

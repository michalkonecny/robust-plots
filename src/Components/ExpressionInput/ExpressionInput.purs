module Components.ExpressionInput where

import Prelude

import Components.ExpressionInput.Controller (ExpressionInputController)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Expression.Error (Expect)
import Expression.Evaluator (roughEvaluate, presetConstants)
import Expression.Syntax (Expression)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
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

data Action
  = Init
  | HandleInput String
  | Parse
  | Status Status

data Status
  = Off
  | Rough
  | Robust

derive instance statusEq :: Eq Status

expressionInputComponent :: forall query m. MonadEffect m => ExpressionInputController -> Int -> H.Component HH.HTML query String ExpressionInputMessage m
expressionInputComponent controller id =
  H.mkComponent
    { initialState: initialState id
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction controller
              , receive = Just <<< HandleInput
              , initialize = Just Init
              }
    }

initialState :: Int -> String -> State
initialState id input =
  { input
  , error: Nothing
  , id
  , status: Robust
  }

render :: forall slots m. State -> HH.ComponentHTML Action slots m
render state =
  HH.div_
    [ HH.input
        [ HP.type_ HP.InputText
        , HE.onValueChange $ toValueChangeActionEvent
        , HP.value state.input
        ]
    , HH.button
        [ HE.onClick $ toActionEvent Parse ]
        [ HH.text "Plot" ]
    , HH.form_
        [ HH.input
            [ HP.type_ HP.InputRadio
            , HE.onChecked $ toCheckedEvent (Status Off)
            , HP.id_ "offCheckBox"
            , HP.checked (state.status == Off)
            ]
        , HH.label
            [ HP.for "offCheckBox" ]
            [ HH.text "Off" ]
        , HH.input
            [ HP.type_ HP.InputRadio
            , HE.onChecked $ toCheckedEvent (Status Rough)
            , HP.id_ "roughCheckBox"
            , HP.checked (state.status == Rough)
            ]
        , HH.label
            [ HP.for "roughCheckBox" ]
            [ HH.text "Rough" ]
        , HH.input
            [ HP.type_ HP.InputRadio
            , HE.onChecked $ toCheckedEvent (Status Robust)
            , HP.id_ "robustCheckBox"
            , HP.checked (state.status == Robust)
            ]
        , HH.label
            [ HP.for "robustCheckBox" ]
            [ HH.text "Rough and Robust" ]
        ]
    , HH.p_
        [ HH.text $ fromMaybe "" state.error ]
    ]

toValueChangeActionEvent :: String -> Maybe Action
toValueChangeActionEvent value = Just $ HandleInput value

toActionEvent :: forall a. Action -> a -> Maybe Action
toActionEvent action _ = Just action

toCheckedEvent :: Action -> Boolean -> Maybe Action
toCheckedEvent action true = Just action
toCheckedEvent action false = Nothing

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
      Right expression -> case checkExpression expression of
        Left evaluationError -> H.modify_ _ { error = Just $ show evaluationError }
        Right _ -> do
          H.modify_ _ { error = Nothing }
          H.raise (Parsed id (controller.clean expression) input)
  HandleInput input -> H.modify_ _ { input = input }

  Status status -> H.modify_ _ { status = status }

checkExpression :: Expression -> Expect Number
checkExpression expression = roughEvaluate (presetConstants <> [ Tuple "x" 0.0 ]) expression

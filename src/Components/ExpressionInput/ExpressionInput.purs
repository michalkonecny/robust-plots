module Components.ExpressionInput where

import Prelude

import Components.ExpressionInput.Controller (ExpressionInputController)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Expression.Error (Expect)
import Expression.Evaluator (evaluate, presetConstants)
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
    }

data ExpressionInputMessage
  = Parsed Expression String

data Action
  = Init
  | HandleInput String
  | Parse

expressionInputComponent :: forall query m. MonadEffect m => ExpressionInputController -> H.Component HH.HTML query String ExpressionInputMessage m
expressionInputComponent controller =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction controller
              , receive = Just <<< HandleInput
              , initialize = Just Init
              }
    }

initialState :: String -> State
initialState input =
  { input
  , error: Nothing
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
      , HH.p_
          [ HH.text $ fromMaybe "" state.error ]
    ]

toValueChangeActionEvent :: String -> Maybe Action
toValueChangeActionEvent value = Just $ HandleInput value

toActionEvent :: forall a. Action -> a -> Maybe Action
toActionEvent action _ = Just action

handleAction :: forall m. MonadEffect m => ExpressionInputController -> Action -> H.HalogenM State Action () ExpressionInputMessage m Unit
handleAction controller = case _ of
  Init -> do
    { input } <- H.get
    handleAction controller (HandleInput input)
    pure unit
  Parse -> do
    { input } <- H.get
    let
      result = controller.parse input
    case result of
      Left parseError -> H.modify_ _ { error = Just $ show parseError }
      Right expression -> case checkExpression expression of
        Left evaluationError -> H.modify_ _ { error = Just $ show evaluationError }
        Right _ -> H.raise (Parsed expression input)
  HandleInput input -> do
    H.modify_ _ { input = input }
    pure unit

checkExpression :: Expression -> Expect Number
checkExpression expression = evaluate (presetConstants <> [ Tuple "x" 0.0 ]) expression
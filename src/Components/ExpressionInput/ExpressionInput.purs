module Components.ExpressionInput where

import Prelude
import Components.Checkbox (CheckboxMessage(..), CheckboxSlot, checkboxComponent)
import Components.ExpressionInput.Controller (ExpressionInputController)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Expression.Syntax (Expression)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

_checkbox = SProxy :: SProxy "checkbox"

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
  | ToggledRobust CheckboxMessage
  | HandleAccuracyInput String
  | ToggledRough CheckboxMessage

data Status
  = Off
  | Rough
  | Robust

derive instance statusEq :: Eq Status

type ChildSlots
  = ( checkbox :: CheckboxSlot Int
    )

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

render :: forall m. MonadEffect m => State -> HH.ComponentHTML Action ChildSlots m
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
              [ HP.class_ (ClassName "input-group mb-3") ]
              [ HH.div
                  [ HP.class_ (ClassName "input-group-prepend") ]
                  [ HH.span [ HP.class_ (ClassName "input-group-text") ] [ HH.text "Accuracy (px)" ] ]
              , HH.input
                  [ HP.type_ HP.InputText
                  , HE.onValueChange $ toValueChangeActionEvent HandleAccuracyInput
                  , HP.value state.accuracyInput
                  , HE.onFocusOut $ toActionEvent UpdateAccuracy
                  , HP.class_ (ClassName "form-control")
                  ]
              ]
          , HH.div_
              [ HH.slot _checkbox 1 (checkboxComponent "Show rough line") (state.input.status == Off) (Just <<< ToggledRough) -- TODO
              , HH.slot _checkbox 2 (checkboxComponent "Show robust enclosures") (state.input.status == Off) (Just <<< ToggledRough) -- TODO
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

errorMessage :: forall m. Maybe String -> Array (HH.ComponentHTML Action ChildSlots m)
errorMessage Nothing = []

errorMessage (Just message) =
  [ HH.div
      [ HP.class_ (ClassName "alert alert-danger") ]
      [ HH.text message ]
  ]

handleAction :: forall m. MonadEffect m => ExpressionInputController -> Action -> H.HalogenM State Action ChildSlots ExpressionInputMessage m Unit
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
  ToggledRough (ToggleChanged status) -> do
    { id } <- H.get
    H.raise (ChangedStatus id if status then Rough else Off) -- TODO
  ToggledRobust (ToggleChanged status) -> do
    { id } <- H.get
    H.raise (ChangedStatus id if status then Robust else Rough) -- TODO

checkValid :: String -> Either Number String
checkValid accuracyString = case fromString accuracyString of
  Nothing -> Right "Failed to parse Accuracy"
  Just accuracy -> Left accuracy

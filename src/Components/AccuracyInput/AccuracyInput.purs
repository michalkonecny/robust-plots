module Components.AccuracyInput where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type AccuracyInputSlot p
  = forall q. H.Slot q AccuracyInputMessage p

type State
  = { accuracy :: String
    }

data AccuracyInputMessage
  = UpdatedAccuracyInput Number

data Action
  = Recieve Number
  | ChangeAccuracy String
  | Update

accuracyInputComponent :: forall query m. MonadEffect m => H.Component HH.HTML query Number AccuracyInputMessage m
accuracyInputComponent =
  H.mkComponent
    { initialState: initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , receive = Just <<< Recieve
              }
    }

initialState :: Number -> State
initialState accuracy =
  { accuracy: show accuracy
  }

render :: forall slots m. State -> HH.ComponentHTML Action slots m
render state =
  HH.div_
    [ HH.label
        [ HP.for "accuracy" ]
        [ HH.text "Accuracy:" ]
    , HH.input
        [ HP.type_ HP.InputText
        , HE.onValueChange $ Just <<< ChangeAccuracy
        , HP.value state.accuracy
        , HP.id_ "accuracy"
        ]
    , HH.button
        [ HE.onClick $ toActionEvent Update ]
        [ HH.text "Update" ]
    , HH.br_
    , HH.p_
        [ HH.text $ outputMessage state.accuracy ]
    ]

outputMessage :: String -> String
outputMessage accuracyString = case checkValid accuracyString of
  Right errorMessage -> errorMessage
  Left _ -> ""

toActionEvent :: forall a. Action -> a -> Maybe Action
toActionEvent action _ = Just action

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () AccuracyInputMessage m Unit
handleAction = case _ of
  ChangeAccuracy stringInput -> H.modify_ _ { accuracy = stringInput }
  Recieve accuracy -> H.modify_ _ { accuracy = show accuracy }
  Update -> do
    state <- H.get
    case checkValid state.accuracy of
      Right errorMessage -> pure unit -- Do nothing
      Left accuracy -> H.raise (UpdatedAccuracyInput accuracy)

checkValid :: String -> Either Number String
checkValid accuracyString = case fromString accuracyString of
  Nothing -> Right "Failed to parse number of accuracyes"
  Just accuracy -> Left accuracy

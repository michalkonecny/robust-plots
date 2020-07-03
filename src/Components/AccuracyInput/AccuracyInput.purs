module Components.AccuracyInput where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type AccuracyInputSlot p
  = forall q. H.Slot q AccuracyInputMessage p

type State
  = { accuracy :: String
    , error :: Maybe String
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
  , error: Nothing
  }

render :: forall slots m. State -> HH.ComponentHTML Action slots m
render state =
  HH.div_
    $ [ HH.div
          [ HP.class_ (ClassName "input-group mb-3") ]
          [ HH.div
              [ HP.class_ (ClassName "input-group-prepend") ]
              [ HH.span [ HP.class_ (ClassName "input-group-text") ] [ HH.text "Accuracy:" ] ]
          , HH.input
              [ HP.type_ HP.InputText
              , HE.onValueChange $ Just <<< ChangeAccuracy
              , HP.value state.accuracy
              , HP.class_ (ClassName "form-control")
              ]
          , HH.button
              [ HE.onClick $ toActionEvent Update, HP.class_ (ClassName "btn btn-info") ]
              [ HH.text "Update" ]
          ]
      ]
    <> (errorMessage state.error)

errorMessage :: forall slots m. Maybe String -> Array (HH.ComponentHTML Action slots m)
errorMessage Nothing = []

errorMessage (Just message) =
  [ HH.div
      [ HP.class_ (ClassName "alert alert-danger") ]
      [ HH.text message ]
  ]

toActionEvent :: forall a. Action -> a -> Maybe Action
toActionEvent action _ = Just action

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () AccuracyInputMessage m Unit
handleAction = case _ of
  ChangeAccuracy stringInput -> H.modify_ _ { accuracy = stringInput }
  Recieve accuracy -> H.modify_ _ { accuracy = show accuracy }
  Update -> do
    state <- H.get
    case checkValid state.accuracy of
      Right error -> H.modify_ _ { error = Just error }
      Left accuracy -> do
        H.modify_ _ { error = Nothing }
        H.raise (UpdatedAccuracyInput accuracy)

checkValid :: String -> Either Number String
checkValid accuracyString = case fromString accuracyString of
  Nothing -> Right "Failed to parse Accuracy"
  Just accuracy -> Left accuracy

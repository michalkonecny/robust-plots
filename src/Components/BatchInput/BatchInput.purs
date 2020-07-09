module Components.BatchInput where

import Prelude

import Components.Common.Action (onClickActionEvent, onValueChangeActionEvent)
import Components.Common.ClassName (className)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type BatchInputSlot p
  = forall q. H.Slot q BatchInputMessage p

type State
  = { batchCount :: String
    , error :: Maybe String
    }

data BatchInputMessage
  = UpdatedBatchInput Int

data Action
  = Recieve Int
  | ChangeBatchCount String
  | Update

batchInputComponent :: forall query m. MonadEffect m => H.Component HH.HTML query Int BatchInputMessage m
batchInputComponent =
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

initialState :: Int -> State
initialState batchCount =
  { batchCount: show batchCount
  , error: Nothing
  }

render :: forall slots m. State -> HH.ComponentHTML Action slots m
render state =
  HH.div_
    $ [ HH.div
          [ className "input-group mb-3" ]
          [ HH.div
              [ className "input-group-prepend" ]
              [ HH.span
                  [ className "input-group-text" ]
                  [ HH.text "Number of Batches:" ]
              ]
          , HH.input
              [ HP.type_ HP.InputText
              , onValueChangeActionEvent ChangeBatchCount
              , HP.value state.batchCount
              , HP.id_ "batchCount"
              , className "form-control"
              ]
          , HH.button
              [ onClickActionEvent Update
              , className "btn btn-primary"
              ]
              [ HH.text "Update" ]
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

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () BatchInputMessage m Unit
handleAction = case _ of
  ChangeBatchCount stringInput -> H.modify_ _ { batchCount = stringInput }
  Recieve batchCount -> H.modify_ _ { batchCount = show batchCount }
  Update -> do
    state <- H.get
    case checkValid state.batchCount of
      Right error -> H.modify_ _ { error = Just error }
      Left batchCount -> do
        H.modify_ _ { error = Nothing }
        H.raise (UpdatedBatchInput batchCount)

checkValid :: String -> Either Int String
checkValid batchCountString = case fromString batchCountString of
  Nothing -> Right "Failed to parse number of batches"
  Just batchCount ->
    if batchCount < one then
      Right "Number of batches must be greater than 1"
    else
      Left batchCount

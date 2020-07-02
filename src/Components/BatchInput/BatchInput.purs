module Components.BatchInput where

import Prelude
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type BatchInputSlot p
  = forall q. H.Slot q BatchInputMessage p

type State
  = { batchCount :: String
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
  }

render :: forall slots m. State -> HH.ComponentHTML Action slots m
render state =
  HH.div_
    [ HH.label
        [ HP.for "batchCount" ]
        [ HH.text "Number of Batches:" ]
    , HH.input
        [ HP.type_ HP.InputText
        , HE.onValueChange $ Just <<< ChangeBatchCount
        , HP.value state.batchCount
        , HP.id_ "batchCount"
        ]
    , HH.button
        [ HE.onClick $ toActionEvent Update ]
        [ HH.text "Update" ]
    , HH.br_
    , HH.p_
        [ HH.text $ outputMessage state.batchCount ]
    ]

outputMessage :: String -> String
outputMessage batchCountString = case checkValid batchCountString of
  Right errorMessage -> errorMessage
  Left _ -> ""

toActionEvent :: forall a. Action -> a -> Maybe Action
toActionEvent action _ = Just action

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () BatchInputMessage m Unit
handleAction = case _ of
  ChangeBatchCount stringInput -> H.modify_ _ { batchCount = stringInput }
  Recieve batchCount -> H.modify_ _ { batchCount = show batchCount }
  Update -> do
    state <- H.get
    case checkValid state.batchCount of
      Right errorMessage -> pure unit -- Do nothing
      Left batchCount -> H.raise (UpdatedBatchInput batchCount)

checkValid :: String -> Either Int String
checkValid batchCountString = case fromString batchCountString of
  Nothing -> Right "Failed to parse number of batches"
  Just batchCount ->
    if batchCount < one then
      Right "Number of batches must be greater than 1"
    else
      Left batchCount

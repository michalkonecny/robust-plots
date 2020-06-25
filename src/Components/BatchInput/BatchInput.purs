module Components.BatchInput where

import Prelude
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type BatchInputSlot p
  = forall q. H.Slot q BatchInputMessage p

type State
  = { batchCount :: String
    , segmentCount :: String
    }

data BatchInputMessage
  = UpdatedBatchInput Int Int

data Action
  = Recieve (Tuple Int Int)
  | ChangeBatchCount String
  | ChangeSegementCount String
  | Update

batchInputComponent :: forall query m. MonadEffect m => H.Component HH.HTML query (Tuple Int Int) BatchInputMessage m
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

initialState :: (Tuple Int Int) -> State
initialState (Tuple batchCount segmentCount) =
  { batchCount: show batchCount
  , segmentCount: show segmentCount
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
    , HH.br_
    , HH.label
        [ HP.for "segmentCount" ]
        [ HH.text "Number of Segments per Batch:" ]
    , HH.input
        [ HP.type_ HP.InputText
        , HE.onValueChange $ Just <<< ChangeSegementCount
        , HP.value state.segmentCount
        , HP.id_ "segmentCount"
        ]
    , HH.br_
    , HH.p_
        [ HH.text $ outputMessage state.batchCount state.segmentCount ]
    , HH.button
        [ HE.onClick $ toActionEvent Update ]
        [ HH.text "Update" ]
    ]

outputMessage :: String -> String -> String
outputMessage batchCountString segmentCountString = case checkValid batchCountString segmentCountString of
  Right errorMessage -> errorMessage
  Left (Tuple batchCount segmentCount) -> "Number of segments: " <> show (batchCount * segmentCount)

toActionEvent :: forall a. Action -> a -> Maybe Action
toActionEvent action _ = Just action

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () BatchInputMessage m Unit
handleAction = case _ of
  ChangeBatchCount stringInput -> H.modify_ _ { batchCount = stringInput }
  ChangeSegementCount stringInput -> H.modify_ _ { segmentCount = stringInput }
  Recieve (Tuple batchCount segmentCount) -> H.modify_ _ { batchCount = show batchCount, segmentCount = show segmentCount }
  Update -> do
    state <- H.get
    case checkValid state.batchCount state.segmentCount of
      Right errorMessage -> pure unit -- Do nothing
      Left (Tuple batchCount segmentCount) -> H.raise (UpdatedBatchInput batchCount segmentCount)

checkValid :: String -> String -> Either (Tuple Int Int) String
checkValid batchCountString segmentCountString = case fromString batchCountString of
  Nothing -> Right "Failed to parse number of batches"
  Just batchCount -> case fromString segmentCountString of
    Nothing -> Right "Failed to parse number of segments per batch"
    Just segmentCount ->
      if batchCount < one then
        Right "Number of batches must be greater than 1"
      else
        if segmentCount < one then
          Right "Number of segments per batch must be greater than 1"
        else
          Left (Tuple batchCount segmentCount)

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
  = { segmentCount :: String
    }

data BatchInputMessage
  = UpdatedBatchInput Int

data Action
  = Recieve Int
  | ChangeSegementCount String
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
initialState segmentCount =
  { segmentCount: show segmentCount
  }

render :: forall slots m. State -> HH.ComponentHTML Action slots m
render state =
  HH.div_
    [ HH.label
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
        [ HH.text $ outputMessage state.segmentCount ]
    , HH.button
        [ HE.onClick $ toActionEvent Update ]
        [ HH.text "Update" ]
    ]

outputMessage :: String -> String
outputMessage segmentCountString = case checkValid segmentCountString of
  Right errorMessage -> errorMessage
  Left segmentCount -> "Number of segments: " <> show segmentCount

toActionEvent :: forall a. Action -> a -> Maybe Action
toActionEvent action _ = Just action

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () BatchInputMessage m Unit
handleAction = case _ of
  ChangeSegementCount stringInput -> H.modify_ _ { segmentCount = stringInput }
  Recieve segmentCount -> H.modify_ _ { segmentCount = show segmentCount }
  Update -> do
    state <- H.get
    case checkValid state.segmentCount of
      Right errorMessage -> pure unit -- Do nothing
      Left segmentCount -> H.raise (UpdatedBatchInput segmentCount)

checkValid :: String -> Either Int String
checkValid segmentCountString = case fromString segmentCountString of
  Nothing -> Right "Failed to parse number of segments per batch"
  Just segmentCount ->
    if segmentCount < one then
      Right "Number of segments per batch must be greater than 1"
    else
      Left segmentCount

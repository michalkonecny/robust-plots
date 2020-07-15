module Components.ProgressBar where

import Prelude
import Components.Common.ClassName (className)
import Components.Common.Styles (style)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH

type ProgressBarSlot p
  = forall q. H.Slot q Void p

type State
  = { isDone :: Boolean
    , progress :: Int
    , total :: Int
    }

type Progress
  = { progress :: Int
    , total :: Int
    }

data Action
  = Recieve Progress

progressBarComponent :: forall query m. MonadEffect m => H.Component HH.HTML query Progress Void m
progressBarComponent =
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

initialState :: Progress -> State
initialState input =
  { isDone: false
  , progress: input.progress
  , total: input.total
  }

render :: forall slots m. State -> HH.ComponentHTML Action slots m
render state =
  HH.div_
    $ if state.isDone then
        []
      else
        [ HH.div
            [ className "progress" ]
            [ HH.div
                [ className "progress-bar", style $ "width: " <> percentString ]
                [ HH.text percentString ]
            ]
        ]
  where
  percent = (state.progress * 100) / state.total

  percentString = (show percent) <> "%"

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () Void m Unit
handleAction (Recieve input) = H.modify_ _ { progress = input.progress, total = input.total, isDone = input.progress == input.total }

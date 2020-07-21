module Components.ProgressBar where

import Prelude
import Components.Common.ClassName (className)
import Components.Common.Styles (style)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (PropName(..))
import Halogen as H
import Halogen.HTML (prop)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type ProgressBarSlot p
  = forall q. H.Slot q Void p

type State
  = { isDone :: Boolean
    , index :: Int
    , total :: Int
    }

type Progress
  = { index :: Int
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
  { isDone: input.index == input.total
  , index: input.index
  , total: input.total
  }

render :: forall slots m. State -> HH.ComponentHTML Action slots m
render state =
  HH.div [ style "min-height: 16px" ]
    $ if state.isDone then
        []
      else
        [ HH.progress
            [ className "progress"
            , style "width: 100%"
            , prop (PropName "value") percent
            , HP.max 100.0
            ]
            []
        ]
  where
  percent = (state.index * 100) / state.total

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () Void m Unit
handleAction (Recieve input) = H.modify_ _ { index = input.index, total = input.total, isDone = input.index == input.total }

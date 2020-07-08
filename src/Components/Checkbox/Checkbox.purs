module Components.Checkbox where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type CheckboxSlot p
  = forall q. H.Slot q CheckboxMessage p

type State
  = { isChecked :: Boolean
    , label :: String
    }

data CheckboxMessage
  = ToggleChanged Boolean

data Action
  = Toggled Boolean
  | HandleMessage Boolean

checkboxComponent :: forall query m. MonadEffect m => String -> H.Component HH.HTML query Boolean CheckboxMessage m
checkboxComponent label =
  H.mkComponent
    { initialState: initialState label
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , receive = Just <<< HandleMessage
              }
    }

initialState :: String -> Boolean -> State
initialState label isChecked = { label, isChecked }

render :: forall slots m. State -> HH.ComponentHTML Action slots m
render state =
  HH.div
    [ HP.class_ (ClassName "form-check form-check-inline") ]
    [ HH.input
        [ HP.type_ HP.InputCheckbox
        , HE.onChecked (Just <<< Toggled)
        , HP.id_ $ state.label <> "CheckBox"
        , HP.checked state.isChecked
        , HP.class_ (ClassName "form-check-input")
        ]
    , HH.label
        [ HP.for $ state.label <> "CheckBox"
        , HP.class_ (ClassName "form-check-label")
        ]
        [ HH.text state.label ]
    ]

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () CheckboxMessage m Unit
handleAction = case _ of
  HandleMessage isChecked -> H.modify_ _ { isChecked = isChecked }
  Toggled status -> H.raise (ToggleChanged status)

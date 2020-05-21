module Components.Canvas where

import Prelude

import Components.Canvas.Context (DrawContext)
import Components.Canvas.CanvasController (CanvasController)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Maybe.Trans as MaybeT
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Types (Size, Domain)

-- COMPONENT
type Slot p
  = forall q. H.Slot q Void p

type State operations
  = { input :: Input operations
    , context :: Maybe DrawContext
    }

type Input operations
  = { operations :: operations
    , size :: Size
    , canvasId :: String
    }

data Action operations
  = Init
  | HandleInput (Input operations)

canvasComponent :: forall operations query output m. MonadEffect m => CanvasController operations -> H.Component HH.HTML query (Input operations) output m
canvasComponent cfg =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction cfg
              , receive = Just <<< HandleInput
              , initialize = Just Init
              }
    }

-- COMPONENT INIT
initialState :: forall operations. Input operations -> State operations
initialState input =
  { input
  , context: Nothing
  }

-- COMPONENT RENDER
render :: forall operations action slots. State operations -> HH.HTML action slots
render { input: { size, canvasId } } =
  HH.canvas
    [ HP.id_ canvasId
    , HP.width $ Int.floor size.width
    , HP.height $ Int.floor size.height
    ]

-- COMPONENT ACTION
handleAction :: forall operations output m. MonadEffect m => CanvasController operations -> Action operations -> H.HalogenM (State operations) (Action operations) () output m Unit
handleAction controller = case _ of
  Init ->
    map (const unit)
      $ runMaybeT do
          { input } <- H.get
          context <- MaybeT $ H.liftEffect $ controller.init input.size
          H.modify_ _ { context = Just context }
          MaybeT.lift $ handleAction controller (HandleInput input)
          pure unit
  HandleInput input -> do
    state <- H.get
    H.modify_ _ { input = input }
    _ <-
      for state.context \context -> do
        when (state.input.size /= input.size) do
          context' <- H.liftEffect $ controller.onResize input.size context
          H.modify_ _ { context = Just context' }
        H.liftEffect $ controller.render context input.operations
    pure unit

domain :: Number -> Number -> Number -> Number -> Domain
domain xLower xUpper yLower yUpper = { xBounds: { upper: xUpper, lower: xLower }, yBounds: { upper: yUpper, lower: yLower } }

module Components.Canvas where

import Prelude
import Components.Canvas.Context (DrawContext)
import Components.Canvas.Controller (CanvasController)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Maybe.Trans as MaybeT
import Data.Int (toNumber)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (Size, XYBounds, Position)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

type CanvasSlot p
  = forall q. H.Slot q CanvasMessage p

type State operations
  = { input :: Input operations
    , context :: Maybe DrawContext
    , oldPosition :: Position
    , isDragging :: Boolean
    }

data CanvasMessage
  = Dragged Position

type Input operations
  = { operations :: operations
    , size :: Size
    , canvasId :: String
    }

data Action operations
  = Init
  | HandleInput (Input operations)
  | StartDrag Position
  | EndDrag
  | Drag Position

canvasComponent :: forall operations query m. MonadEffect m => CanvasController operations -> H.Component HH.HTML query (Input operations) CanvasMessage m
canvasComponent controller =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction controller
              , receive = Just <<< HandleInput
              , initialize = Just Init
              }
    }

initialState :: forall operations. Input operations -> State operations
initialState input =
  { input
  , context: Nothing
  , oldPosition: { x: 0.0, y: 0.0 }
  , isDragging: false
  }

render :: forall operations slots m. State operations -> HH.ComponentHTML (Action operations) slots m
render { input: { size, canvasId } } =
  HH.canvas
    [ HP.id_ canvasId
    , HP.width $ Int.floor size.width
    , HP.height $ Int.floor size.height
    , HE.onMouseDown $ (\(event :: MouseEvent) -> Just $ StartDrag { x: toNumber $ ME.clientX event, y: toNumber $ ME.clientY event })
    , HE.onMouseMove $ (\(event :: MouseEvent) -> Just $ Drag { x: toNumber $ ME.clientX event, y: toNumber $ ME.clientY event })
    , HE.onMouseUp $ (\(event :: MouseEvent) -> Just $ EndDrag)
    ]

handleAction :: forall operations m. MonadEffect m => CanvasController operations -> Action operations -> H.HalogenM (State operations) (Action operations) () CanvasMessage m Unit
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
  StartDrag position -> do
    H.modify_ _ { oldPosition = position, isDragging = true }
  EndDrag -> H.modify_ _ { isDragging = false }
  Drag position -> do
    state <- H.get
    when state.isDragging do
      H.put $ state { oldPosition = position }
      H.raise $ Dragged { x: state.oldPosition.x - position.x, y: position.y - state.oldPosition.y }

xyBounds :: Number -> Number -> Number -> Number -> XYBounds
xyBounds xLower xUpper yLower yUpper = { xBounds: { upper: xUpper, lower: xLower }, yBounds: { upper: yUpper, lower: yLower } }

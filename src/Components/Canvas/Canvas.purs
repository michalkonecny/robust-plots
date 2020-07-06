module Components.Canvas where

import Prelude
import Components.Canvas.Context (DrawContext)
import Components.Canvas.Controller (CanvasController)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Maybe.Trans as MaybeT
import Data.Int (round)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import IntervalArith.Misc (rationalToNumber, toRational)
import Types (Delta, Size)
import Web.HTML (window) as Web
import Web.HTML.Window (document) as Web
import Web.HTML.HTMLDocument (toNonElementParentNode, HTMLDocument)
import Web.HTML.HTMLElement (offsetWidth, HTMLElement, fromElement)
import Web.DOM.NonElementParentNode (getElementById)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

type CanvasSlot p
  = forall q. H.Slot q CanvasMessage p

type State operations
  = { input :: Input operations
    , context :: Maybe DrawContext
    , oldDelta :: Delta
    , isDragging :: Boolean
    }

data CanvasMessage
  = Dragged Delta
  | StoppedDragging

type Input operations
  = { operations :: operations
    , size :: Size
    , canvasId :: String
    }

data Action operations
  = Init
  | HandleInput (Input operations)
  | StartDrag Delta
  | EndDrag
  | Drag Delta

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
  , oldDelta: { x: zero, y: zero }
  , isDragging: false
  }

render :: forall operations slots m. State operations -> HH.ComponentHTML (Action operations) slots m
render { input: { size, canvasId } } =
  HH.canvas
    [ HP.id_ canvasId
    , HP.width $ Int.floor $ rationalToNumber size.width
    , HP.height $ Int.floor $ rationalToNumber size.height
    , HE.onMouseDown $ (\(event :: MouseEvent) -> Just $ StartDrag { x: toRational $ ME.clientX event, y: toRational $ ME.clientY event })
    , HE.onMouseMove $ (\(event :: MouseEvent) -> Just $ Drag { x: toRational $ ME.clientX event, y: toRational $ ME.clientY event })
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
  StartDrag delta -> do
    H.modify_ _ { oldDelta = delta, isDragging = true }
  EndDrag -> do
    H.modify_ _ { isDragging = false }
    H.raise StoppedDragging
  Drag delta -> do
    state <- H.get
    when state.isDragging do
      H.put $ state { oldDelta = delta }
      H.raise $ Dragged { x: state.oldDelta.x - delta.x, y: delta.y - state.oldDelta.y }

calculateNewCanvasSize :: Effect (Maybe Size)
calculateNewCanvasSize = do
  document <- Web.document =<< Web.window
  maybeCanvasContainer <- findElementById "canvasContainer" document
  case maybeCanvasContainer of
    Nothing -> pure $ Nothing
    Just container -> do
      containerWidth <- offsetWidth container
      let
        newWidth = containerWidth - 40.0 -- Account for padding

        newHeight = (newWidth * 5.0) / 8.0
      pure $ Just { width: toRational (round newWidth), height: toRational (round newHeight) }

findElementById :: String -> HTMLDocument -> Effect (Maybe HTMLElement)
findElementById id document = do
  maybeElement <- getElementById id $ toNonElementParentNode document
  case maybeElement of
    Nothing -> pure Nothing
    Just element -> pure $ fromElement element

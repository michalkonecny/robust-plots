module Main where

import Prelude
import Components.Canvas (Input, CanvasSlot, canvasComponent, xyBounds)
import Components.Canvas.Controller (canvasController)
import Components.ExpressionInput (ExpressionInputSlot, ExpressionInputMessage(..), expressionInputComponent)
import Components.ExpressionInput.Controller (expressionInputController)
import Constants (canvasId)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..), isJust)
import Data.Symbol (SProxy(..))
import Draw.Commands (DrawCommand)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Expression.Syntax (Expression)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource as ES
import Halogen.VDom.Driver (runUI)
import Plot.Commands (PlotCommand, plot, clear)
import Plot.Pan (panBounds)
import Plot.PlotController (computePlotAsync)
import Plot.Zoom (zoomBounds)
import Types (Direction(..), Size, XYBounds)
import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.WheelEvent as WE
import Web.UIEvent.WheelEvent.EventTypes as WET

type Config
  = { someData :: String }

type State
  = { input :: Input (DrawCommand Unit)
    , bounds :: XYBounds
    , expression :: Maybe Expression
    , expressionText :: String
    }

data Action
  = Clear
  | Init
  | Pan Direction
  | Zoom Boolean
  | HandleExpressionInput ExpressionInputMessage
  | HandleScroll H.SubscriptionId WheelEvent

type ChildSlots
  = ( canvas :: CanvasSlot Int
    , expressionInput :: ExpressionInputSlot Int
    )

_canvas = SProxy :: SProxy "canvas"

_expressionInput = SProxy :: SProxy "expressionInput"

ui :: forall f i o. H.Component HH.HTML f i o (ReaderT Config Aff)
ui =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Init
              }
    }
  where
  initialState :: State
  initialState =
    { input:
        { operations: pure unit
        , canvasId: canvasId
        , size:
            { width: 800.0
            , height: 500.0
            }
        }
    , bounds: xyBounds (-1.0) (1.0) (-1.0) (1.0)
    , expressionText: ""
    , expression: Nothing
    }

  render :: forall m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Robust plot" ]
      , HH.slot _expressionInput 1 (expressionInputComponent expressionInputController) state.expressionText (Just <<< HandleExpressionInput)
      , HH.button
          [ HE.onClick $ toActionEvent Clear ]
          [ HH.text "Clear plot" ]
      , HH.button
          [ HE.onClick $ toActionEvent $ Pan Left ]
          [ HH.text "◄" ]
      , HH.button
          [ HE.onClick $ toActionEvent $ Pan Right ]
          [ HH.text "►" ]
      , HH.button
          [ HE.onClick $ toActionEvent $ Pan Down ]
          [ HH.text "▼" ]
      , HH.button
          [ HE.onClick $ toActionEvent $ Pan Up ]
          [ HH.text "▲" ]
      , HH.button
          [ HE.onClick $ toActionEvent $ Zoom true ]
          [ HH.text "+" ]
      , HH.button
          [ HE.onClick $ toActionEvent $ Zoom false ]
          [ HH.text "-" ]
      , HH.slot _canvas 1 (canvasComponent canvasController) state.input absurd
      ]

toActionEvent :: forall a. Action -> a -> Maybe Action
toActionEvent action _ = Just action

computePlot :: Size -> PlotCommand -> ReaderT Config Aff (DrawCommand Unit)
computePlot canvasSize plot = lift $ computePlotAsync canvasSize plot

handleAction :: forall o. Action -> H.HalogenM State Action ChildSlots o (ReaderT Config Aff) Unit
handleAction action = do
  state <- H.get
  case action of
    Init -> do
      drawCommands <- lift $ computePlot state.input.size $ clear state.bounds
      document <- H.liftEffect $ Web.document =<< Web.window
      H.subscribe' \id ->
        ES.eventListenerEventSource
          WET.wheel
          (HTMLDocument.toEventTarget document)
          (map (HandleScroll id) <<< WE.fromEvent)
      H.put state { input { operations = drawCommands } }
    Clear -> do
      drawCommands <- lift $ computePlot state.input.size $ clear state.bounds
      H.put state { input { operations = drawCommands } }
    HandleExpressionInput (Parsed expression text) -> do
      drawCommands <- lift $ computePlot state.input.size $ plot (isJust state.expression) state.bounds expression
      H.put state { input { operations = drawCommands }, expressionText = text, expression = Just expression }
    Pan direction -> do
      let
        newBounds = panBounds state.bounds direction
      drawCommands <- lift $ recomputePlot state newBounds
      H.put state { input { operations = drawCommands }, bounds = newBounds }
    Zoom isZoomIn -> do
      let
        newBounds = zoomBounds state.bounds isZoomIn
      drawCommands <- lift $ recomputePlot state newBounds
      H.put state { input { operations = drawCommands }, bounds = newBounds }
    HandleScroll _ event -> do
      let
        changeInY = WE.deltaY event
      when (changeInY /= 0.0) do
        H.liftEffect $ E.preventDefault (WE.toEvent event)
        handleAction $ Zoom (changeInY < 0.0)

ui' :: forall f i o. H.Component HH.HTML f i o Aff
ui' = H.hoist (\app -> runReaderT app initialConfig) ui

recomputePlot :: State -> XYBounds -> ReaderT Config Aff (DrawCommand Unit)
recomputePlot state newBounds = case state.expression of
  Just expression -> computePlot state.input.size $ plot true newBounds expression
  Nothing -> computePlot state.input.size $ clear newBounds

initialConfig :: Config
initialConfig = { someData: "" }

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI ui' unit body

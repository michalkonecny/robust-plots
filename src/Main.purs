module Main where

import Prelude
import Components.BoundsInput (BoundsInputMessage(..), BoundsInputSlot, boundsInputComponent)
import Components.Canvas (Input, CanvasSlot, CanvasMessage(..), canvasComponent, xyBounds)
import Components.Canvas.Controller (canvasController)
import Components.ExpressionInput (ExpressionInputSlot, ExpressionInputMessage(..), expressionInputComponent)
import Components.ExpressionInput.Controller (expressionInputController)
import Constants (canvasId)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array (length, mapMaybe, null)
import Data.Maybe (Maybe(..))
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
import Plot.Commands (PlotCommand, plotExpression, clear)
import Plot.Pan (panBounds, panBoundsByVector)
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
    , plots :: Array ExpressionPlot
    }

type ExpressionPlot
  = { expression :: Maybe Expression
    , expressionText :: String
    , id :: Int
    }

data Action
  = Clear
  | Init
  | Pan Direction
  | Zoom Boolean
  | HandleExpressionInput ExpressionInputMessage
  | HandleScroll H.SubscriptionId WheelEvent
  | HandleCanvas CanvasMessage
  | HandleBoundsInput BoundsInputMessage
  | AddPlot
  | ResetBounds

type ChildSlots
  = ( canvas :: CanvasSlot Int
    , expressionInput :: ExpressionInputSlot Int
    , boundsInput :: BoundsInputSlot Int
    )

_canvas = SProxy :: SProxy "canvas"

_expressionInput = SProxy :: SProxy "expressionInput"

_boundsInput = SProxy :: SProxy "boundsInput"

ui :: forall query input output. H.Component HH.HTML query input output (ReaderT Config Aff)
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
    , bounds: initialBounds
    , plots:
        [ newPlot 1
        ]
    }

  render :: forall m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      $ [ HH.h1_
            [ HH.text "Robust plot" ]
        ]
      <> inputs
      <> [ HH.button
            [ HE.onClick $ toActionEvent $ AddPlot ]
            [ HH.text "Add plot" ]
        , HH.button
            [ HE.onClick $ toActionEvent Clear ]
            [ HH.text "Clear plots" ]
        , HH.slot _boundsInput 1 boundsInputComponent state.bounds (Just <<< HandleBoundsInput)
        , HH.button
            [ HE.onClick $ toActionEvent $ ResetBounds ]
            [ HH.text "Reset" ]
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
        , HH.slot _canvas 1 (canvasComponent canvasController) state.input (Just <<< HandleCanvas)
        ]
    where
    inputs = map (\plot -> HH.slot _expressionInput plot.id (expressionInputComponent expressionInputController plot.id) plot.expressionText (Just <<< HandleExpressionInput)) state.plots

toActionEvent :: forall a. Action -> a -> Maybe Action
toActionEvent action _ = Just action

handleAction :: forall output. Action -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
handleAction action = do
  state <- H.get
  case action of
    Init -> do
      document <- H.liftEffect $ Web.document =<< Web.window
      H.subscribe' \id ->
        ES.eventListenerEventSource
          WET.wheel
          (HTMLDocument.toEventTarget document)
          (map (HandleScroll id) <<< WE.fromEvent)
      handleAction Clear
    Clear -> do
      let
        plots = map (\plot -> plot { expression = Nothing }) state.plots
      drawCommands <- lift $ computePlots state.input.size state.bounds plots
      H.put state { input { operations = drawCommands }, plots = plots }
    HandleExpressionInput (Parsed id expression text) -> do
      let
        plots = map (updatePlot id expression text) state.plots
      drawCommands <- lift $ computePlots state.input.size state.bounds plots
      H.put state { input { operations = drawCommands }, plots = plots }
    Pan direction -> do
      let
        newBounds = panBounds state.bounds direction
      drawCommands <- lift $ computePlots state.input.size newBounds state.plots
      H.put state { input { operations = drawCommands }, bounds = newBounds }
    Zoom isZoomIn -> do
      let
        newBounds = zoomBounds state.bounds isZoomIn
      drawCommands <- lift $ computePlots state.input.size newBounds state.plots
      H.put state { input { operations = drawCommands }, bounds = newBounds }
    ResetBounds -> do
      drawCommands <- lift $ computePlots state.input.size initialBounds state.plots
      H.put state { input { operations = drawCommands }, bounds = newBounds }
    HandleScroll _ event -> do
      let
        changeInY = WE.deltaY event
      when (changeInY /= 0.0) do
        H.liftEffect $ E.preventDefault (WE.toEvent event)
        handleAction $ Zoom (changeInY < 0.0)
    HandleCanvas (Dragged delta) -> do
      let
        newBounds = panBoundsByVector state.input.size state.bounds delta
      drawCommands <- lift $ computePlots state.input.size newBounds state.plots
      H.put state { input { operations = drawCommands }, bounds = newBounds }
    AddPlot -> do
      let
        plots = state.plots <> [ newPlot (1 + length state.plots) ]
      H.put state { plots = plots }
    HandleBoundsInput (Updated newBounds) -> do
      drawCommands <- lift $ computePlots state.input.size newBounds state.plots
      H.put state { input { operations = drawCommands }, bounds = newBounds }

ui' :: forall f i o. H.Component HH.HTML f i o Aff
ui' = H.hoist (\app -> runReaderT app initialConfig) ui

newPlot :: Int -> ExpressionPlot
newPlot id = { expressionText: "", expression: Nothing, id }

updatePlot :: Int -> Expression -> String -> ExpressionPlot -> ExpressionPlot
updatePlot id expression text plot =
  if plot.id == id then
    { expressionText: text, expression: Just expression, id }
  else
    plot

initialBounds :: XYBounds
initialBounds = xyBounds (-1.0) (1.0) (-1.0) (1.0)

computePlots :: Size -> XYBounds -> Array ExpressionPlot -> ReaderT Config Aff (DrawCommand Unit)
computePlots canvasSize newBounds plots = lift $ computePlotAsync canvasSize computedPlot
  where
  commands = mapMaybe (computePlot canvasSize newBounds) plots

  clearCommand = clear newBounds

  computedPlot =
    if null commands then
      [ clearCommand ]
    else
      [ clearCommand ] <> commands

computePlot :: Size -> XYBounds -> ExpressionPlot -> Maybe PlotCommand
computePlot size newBounds plot = case plot.expression of
  Just expression -> Just $ plotExpression newBounds expression
  Nothing -> Nothing

initialConfig :: Config
initialConfig = { someData: "" }

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI ui' unit body

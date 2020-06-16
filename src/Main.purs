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
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (class MonadEffect)
import Expression.Syntax (Expression)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource as ES
import Halogen.VDom.Driver (runUI)
import IntervalArith.Misc (toRational)
import Plot.Commands (PlotCommand, clear, robustPlot, roughPlot)
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
    , commandSetId :: Int
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
  | StartRobust Int (Array PlotCommand) XYBounds
  | EndRobust Int (DrawCommand Unit)

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
            { width: toRational 800
            , height: toRational 500
            }
        }
    , bounds: initialBounds
    , plots:
        [ newPlot 1
        ]
    , commandSetId: 0
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
    Clear -> plotRoughThenRobust state state.bounds (map (\plot -> plot { expression = Nothing }) state.plots)
    HandleExpressionInput (Parsed id expression text) -> plotRoughThenRobust state state.bounds (map (updatePlot id expression text) state.plots)
    Pan direction -> plotRoughThenRobust state (panBounds state.bounds direction) state.plots
    Zoom isZoomIn -> plotRoughThenRobust state (zoomBounds state.bounds isZoomIn) state.plots
    ResetBounds -> plotRoughThenRobust state initialBounds state.plots
    HandleCanvas (Dragged delta) -> plotRoughThenRobust state (panBoundsByVector state.input.size state.bounds delta) state.plots
    AddPlot -> H.put state { plots = state.plots <> [ newPlot (1 + length state.plots) ] }
    HandleBoundsInput (Updated newBounds) -> plotRoughThenRobust state newBounds state.plots
    Init -> do
      document <- H.liftEffect $ Web.document =<< Web.window
      H.subscribe' \id ->
        ES.eventListenerEventSource
          WET.wheel
          (HTMLDocument.toEventTarget document)
          (map (HandleScroll id) <<< WE.fromEvent)
      handleAction Clear
    HandleScroll _ event -> do
      let
        changeInY = WE.deltaY event
      when (changeInY /= 0.0) do
        H.liftEffect $ E.preventDefault (WE.toEvent event)
        handleAction $ Zoom (changeInY < 0.0)
    StartRobust id robustCommands bounds -> do
      _ <- lift $ lift $ delay $ Milliseconds 2000.0 -- TODO Remove artifical delay
      robustDrawCommands <- lift $ computePlots state.input.size robustCommands
      handleAction $ EndRobust id robustDrawCommands
    EndRobust id drawCommands ->
      -- If the command set is different then ignore the drawCommands
      when (state.commandSetId == id) do
        H.put state { input { operations = drawCommands } }

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
initialBounds = xyBounds (-one) (one) (-one) (one)

plotRoughThenRobust :: forall output. State -> XYBounds -> Array ExpressionPlot -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
plotRoughThenRobust state newBounds plots = do
  let
    toCommandArray = plotCommands state.input.size newBounds plots

    roughCommands = toCommandArray roughPlot

    robustCommands = toCommandArray robustPlot

    commandSetId = state.commandSetId + 1
  drawCommands <- lift $ computePlots state.input.size roughCommands
  H.put state { input { operations = drawCommands }, plots = plots, bounds = newBounds, commandSetId = commandSetId }
  handleAction $ StartRobust commandSetId robustCommands newBounds

computePlots :: Size -> Array PlotCommand -> ReaderT Config Aff (DrawCommand Unit)
computePlots canvasSize plots = lift $ computePlotAsync canvasSize plots

toMaybePlotCommand :: Size -> XYBounds -> (XYBounds -> Expression -> String -> PlotCommand) -> ExpressionPlot -> Maybe PlotCommand
toMaybePlotCommand size newBounds plotter plot = case plot.expression of
  Just expression -> Just $ plotter newBounds expression plot.expressionText
  Nothing -> Nothing

plotCommands :: Size -> XYBounds -> Array ExpressionPlot -> (XYBounds -> Expression -> String -> PlotCommand) -> Array PlotCommand
plotCommands canvasSize newBounds plots plotter = computedPlot
  where
  commands = mapMaybe (toMaybePlotCommand canvasSize newBounds plotter) plots

  clearCommand = clear newBounds

  computedPlot = if null commands then [ clearCommand ] else [ clearCommand ] <> commands

initialConfig :: Config
initialConfig = { someData: "" }

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI ui' unit body

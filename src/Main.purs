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
import Data.Array (fold, length, mapMaybe)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Draw.Commands (DrawCommand)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Expression.Syntax (Expression)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource as ES
import Halogen.VDom.Driver (runUI)
import IntervalArith.Misc (toRational)
import Plot.Commands (PlotCommand, clear, robustPlot, roughPlot)
import Plot.JobBatcher (JobQueue, JobResult, addManyPlots, addPlot, cancelAll, cancelWithBatchId, hasJobs, initialJobQueue, isCancelled, pop, runFirst, showQueueIds)
import Plot.Pan (panBounds, panBoundsByVector)
import Plot.Zoom (zoomBounds)
import Types (Direction(..), Id, XYBounds)
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
    , clearPlot :: DrawCommand Unit
    , queue :: JobQueue
    }

type ExpressionPlot
  = { expression :: Maybe Expression
    , expressionText :: String
    , drawCommands :: DrawCommand Unit
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
  | DrawPlot
  | HandleQueue

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
    , clearPlot: pure unit
    , queue: initialJobQueue
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

clearPlotBatchId :: Id
clearPlotBatchId = 0

handleAction :: forall output. Action -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
handleAction action = do
  state <- H.get
  case action of
    Clear -> do
      let
        canceledQueue = cancelAll state.queue

        queueWithClear = addPlot canceledQueue (clear state.bounds) clearPlotBatchId
      H.modify_ (_ { plots = [ newPlot 1 ], clearPlot = pure unit, queue = queueWithClear })
      handleAction HandleQueue
    HandleExpressionInput (Parsed id expression text) -> do
      let
        plots = alterPlot (updateExpressionPlotInfo expression text) id state.plots

        updatedQueue = cancelWithBatchId state.queue id

        rough = roughPlot state.bounds expression text

        robust = robustPlot state.bounds expression text

        withRough = addPlot updatedQueue rough id

        withRobust = addPlot withRough robust id
      H.modify_ (_ { plots = plots, queue = withRobust })
      handleAction HandleQueue
    Pan direction -> redrawWithBounds state (panBounds state.bounds direction)
    Zoom isZoomIn -> redrawWithBounds state (zoomBounds state.bounds isZoomIn)
    ResetBounds -> redrawWithBounds state initialBounds
    HandleCanvas (Dragged delta) -> redrawWithBounds state (panBoundsByVector state.input.size state.bounds delta)
    AddPlot -> H.modify_ (_ { plots = state.plots <> [ newPlot (1 + length state.plots) ] })
    HandleBoundsInput (Updated newBounds) -> redrawWithBounds state newBounds
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
    DrawPlot -> H.modify_ (_ { input { operations = foldDrawCommands state } })
    HandleQueue -> do
      H.liftEffect $ log $ showQueueIds state.queue
      when (hasJobs state.queue) do
        H.modify_ (_ { queue = pop state.queue })
        maybeJobResult <- lift $ lift $ runFirst state.input.size state.bounds.xBounds state.queue
        _ <- lift $ lift $ delay $ Milliseconds 2000.0 -- TODO Remove artifical delay
        newState <- H.get
        handleJobResult maybeJobResult newState

ui' :: forall f i o. H.Component HH.HTML f i o Aff
ui' = H.hoist (\app -> runReaderT app initialConfig) ui

handleJobResult :: forall output. Maybe JobResult -> State -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
handleJobResult Nothing _ = pure unit

handleJobResult (Just jobResult) newState =
  if isCancelled newState.queue jobResult.job.id then
    pure unit
  else do
    if jobResult.job.batchId == clearPlotBatchId then
      H.modify_ (_ { clearPlot = jobResult.drawCommands })
    else
      H.modify_ (_ { plots = alterPlot (updateExpressionPlotCommands jobResult.drawCommands) jobResult.job.batchId newState.plots })
    handleAction DrawPlot
    handleAction HandleQueue

newPlot :: Int -> ExpressionPlot
newPlot id = { expressionText: "", expression: Nothing, id, drawCommands: pure unit }

updateExpressionPlotInfo :: Expression -> String -> ExpressionPlot -> ExpressionPlot
updateExpressionPlotInfo expression text = _ { expressionText = text, expression = Just expression, drawCommands = pure unit }

updateExpressionPlotCommands :: DrawCommand Unit -> ExpressionPlot -> ExpressionPlot
updateExpressionPlotCommands commands plot = plot { drawCommands = fold [ plot.drawCommands, commands ] }

alterPlot :: (ExpressionPlot -> ExpressionPlot) -> Id -> Array ExpressionPlot -> Array ExpressionPlot
alterPlot alterF id = map mapper
  where
  mapper :: ExpressionPlot -> ExpressionPlot
  mapper plot = if plot.id == id then alterF plot else plot

initialBounds :: XYBounds
initialBounds = xyBounds (-one) one (-one) one

redrawWithBounds :: forall output. State -> XYBounds -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
redrawWithBounds state newBounds = do
  let
    canceledQueue = cancelAll state.queue

    queueWithClear = addPlot canceledQueue (clear newBounds) clearPlotBatchId

    plots = map (_ { drawCommands = pure unit }) state.plots

    roughPlotsWithId = mapMaybe (toMaybePlotCommandWithId newBounds roughPlot) plots

    robustPlotsWithId = mapMaybe (toMaybePlotCommandWithId newBounds robustPlot) plots

    withRough = addManyPlots queueWithClear roughPlotsWithId

    withRobust = addManyPlots withRough robustPlotsWithId
  H.modify_ (_ { plots = plots, queue = withRobust, bounds = newBounds })
  handleAction HandleQueue

toMaybePlotCommandWithId :: XYBounds -> (XYBounds -> Expression -> String -> PlotCommand) -> ExpressionPlot -> Maybe (Tuple PlotCommand Id)
toMaybePlotCommandWithId newBounds plotter plot = case plot.expression of
  Just expression -> Just $ Tuple (plotter newBounds expression plot.expressionText) plot.id
  Nothing -> Nothing

toMaybeDrawCommand :: ExpressionPlot -> Maybe (DrawCommand Unit)
toMaybeDrawCommand plot = case plot.expression of
  Just expression -> Just plot.drawCommands
  Nothing -> Nothing

foldDrawCommands :: State -> (DrawCommand Unit)
foldDrawCommands state = fold $ [ state.clearPlot ] <> mapMaybe toMaybeDrawCommand state.plots

initialConfig :: Config
initialConfig = { someData: "" }

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI ui' unit body

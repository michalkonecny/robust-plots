module Components.Main.Action where

import Prelude
import Components.BoundsInput (BoundsInputMessage(..))
import Components.Canvas (CanvasMessage(..), xyBounds)
import Components.ExpressionInput (ExpressionInputMessage(..))
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array (fold, length, mapMaybe)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Draw.Commands (DrawCommand)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class.Console (log)
import Expression.Syntax (Expression)
import Halogen as H
import Halogen.Query.EventSource as ES
import Plot.Commands (PlotCommand, clear, robustPlot, roughPlot)
import Plot.JobBatcher (JobResult, addManyPlots, addPlot, cancelAll, cancelWithBatchId, hasJobs, isCancelled, pop, runFirst, showQueueIds)
import Plot.Pan (panBounds, panBoundsByVector)
import Plot.Zoom (zoomBounds)
import Types (Direction, Id, XYBounds)
import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.WheelEvent as WE
import Web.UIEvent.WheelEvent.EventTypes as WET
import Components.Main.Types (ChildSlots, Config, ExpressionPlot, State)

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

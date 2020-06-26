module Components.Main.Action where

import Prelude
import Components.BatchInput (BatchInputMessage(..))
import Components.BoundsInput (BoundsInputMessage(..))
import Components.Canvas (CanvasMessage(..))
import Components.ExpressionInput (ExpressionInputMessage(..))
import Components.Main.Helper (alterPlot, foldDrawCommands, initialBounds, newPlot, toMaybePlotCommandWithId, updateExpressionPlotCommands, updateExpressionPlotInfo)
import Components.Main.Types (ChildSlots, Config, State, ExpressionPlot)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parSequence)
import Data.Array (length, mapMaybe)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Halogen as H
import Halogen.Query.EventSource as ES
import Plot.Commands (clear, robustPlot, roughPlot)
import Plot.JobBatcher (JobResult, addManyPlots, addPlot, cancelAll, cancelWithBatchId, clearCancelled, hasJobs, isCancelled, runFirst, setRunning)
import Plot.Pan (panBounds, panBoundsByVector)
import Plot.PlotController (computePlotAsync)
import Plot.Zoom (zoomBounds)
import Types (Direction, Size, XYBounds)
import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.WheelEvent as WE
import Web.UIEvent.WheelEvent.EventTypes as WET

data Action
  = Clear
  | Init
  | Pan Direction
  | Zoom Boolean
  | HandleExpressionInput ExpressionInputMessage
  | HandleScroll H.SubscriptionId WheelEvent
  | HandleCanvas CanvasMessage
  | HandleBoundsInput BoundsInputMessage
  | HandleBatchInput BatchInputMessage
  | AddPlot
  | ResetBounds
  | DrawPlot
  | HandleQueue

handleAction :: forall output. Action -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
handleAction action = do
  state <- H.get
  case action of
    Clear -> do
      clearBounds <- lift $ lift $ computePlotAsync state.input.size (clear state.bounds)
      H.modify_ (_ { plots = [ newPlot 1 ], clearPlot = clearBounds, queue = cancelAll state.queue })
      handleAction DrawPlot
    HandleExpressionInput (Parsed id expression text) -> do
      newRoughCommands <- lift $ lift $ computePlotAsync state.input.size (roughPlot state.bounds expression text)
      let
        plots = alterPlot (updateExpressionPlotInfo expression text newRoughCommands) id state.plots

        updatedQueue = cancelWithBatchId state.queue id

        robust = robustPlot state.segmentCount state.bounds expression text

        withRobust = addPlot state.batchCount updatedQueue robust id
      H.modify_ (_ { plots = plots, queue = withRobust })
      handleAction HandleQueue
    Pan direction -> do
      redrawWithoutRobustWithBounds state (panBounds state.bounds direction)
      _ <- forkWithDelay 500.0 -- Subsiquent code is placed on the end of the JS event queue
      newState <- H.get
      redraw newState
    Zoom isZoomIn -> do
      redrawWithoutRobustWithBounds state (zoomBounds state.bounds isZoomIn)
      _ <- forkWithDelay 500.0 -- Subsiquent code is placed on the end of the JS event queue
      newState <- H.get
      redraw newState
    ResetBounds -> redrawWithBounds state initialBounds
    HandleCanvas (Dragged delta) -> redrawWithoutRobustWithBounds state (panBoundsByVector state.input.size state.bounds delta)
    HandleCanvas StoppedDragging -> redraw state
    AddPlot -> H.modify_ (_ { plots = state.plots <> [ newPlot (1 + length state.plots) ] })
    HandleBoundsInput (UpdatedBoundsInput newBounds) -> redrawWithBounds state newBounds
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
      if (hasJobs state.queue) then do
        H.modify_ (_ { queue = setRunning state.queue })
        maybeJobResult <- lift $ lift $ runFirst state.input.size state.bounds.xBounds state.queue
        _ <- fork -- Subsiquent code is placed on the end of the JS event queue
        newState <- H.get
        handleJobResult maybeJobResult newState
      else
        H.modify_ (_ { queue = clearCancelled state.queue })
    HandleBatchInput (UpdatedBatchInput batchCount segmentCount) -> do
      H.modify_ (_ { batchCount = batchCount, segmentCount = segmentCount })
      redraw state { batchCount = batchCount, segmentCount = segmentCount }

handleJobResult :: forall output. Maybe JobResult -> State -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
handleJobResult Nothing _ = pure unit

handleJobResult (Just jobResult) newState =
  if isCancelled newState.queue jobResult.job.id then
    pure unit
  else do
    H.modify_ (_ { plots = alterPlot (updateExpressionPlotCommands jobResult.drawCommands) jobResult.job.batchId newState.plots })
    handleAction DrawPlot
    handleAction HandleQueue

forkWithDelay :: forall output. Number -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
forkWithDelay duration = lift $ lift $ delay $ Milliseconds duration

fork :: forall output. H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
fork = forkWithDelay 0.0

redraw :: forall output. State -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
redraw state = do
  let
    canceledQueue = cancelAll state.queue

    robustPlotsWithId = mapMaybe (toMaybePlotCommandWithId state.segmentCount state.bounds) state.plots

    withRobust = addManyPlots state.batchCount canceledQueue robustPlotsWithId
  plots <- lift $ lift $ parSequence $ map (computeExpressionPlot state.input.size state.bounds) state.plots
  clearBounds <- lift $ lift $ computePlotAsync state.input.size (clear state.bounds)
  H.modify_ (_ { plots = plots, queue = withRobust, clearPlot = clearBounds })
  handleAction DrawPlot
  handleAction HandleQueue

redrawWithBounds :: forall output. State -> XYBounds -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
redrawWithBounds state newBounds = do
  H.modify_ (_ { bounds = newBounds })
  redraw state { bounds = newBounds }

redrawWithoutRobustWithBounds :: forall output. State -> XYBounds -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
redrawWithoutRobustWithBounds state newBounds = do
  plots <- lift $ lift $ parSequence $ map (computeExpressionPlot state.input.size newBounds) state.plots
  clearBounds <- lift $ lift $ computePlotAsync state.input.size (clear newBounds)
  H.modify_ (_ { plots = plots, queue = cancelAll state.queue, clearPlot = clearBounds, bounds = newBounds })
  handleAction DrawPlot

computeExpressionPlot :: Size -> XYBounds -> ExpressionPlot -> Aff (ExpressionPlot)
computeExpressionPlot size newBounds plot = case plot.expression of
  Nothing -> pure plot
  Just expression -> do
    drawCommands <- computePlotAsync size $ roughPlot newBounds expression plot.expressionText
    pure $ plot { drawCommands = drawCommands }

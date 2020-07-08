module Components.Main.Action where

import Prelude
import Components.BatchInput (BatchInputMessage(..))
import Components.BoundsInput (BoundsInputMessage(..))
import Components.Canvas (CanvasMessage(..), calculateNewCanvasSize)
import Components.ExpressionInput (ExpressionInputMessage(..))
import Components.ExpressionManager (ExpressionManagerMessage(..))
import Components.ExpressionManager.Types (ExpressionPlot)
import Components.Main.Helper (alterPlot, anyPlotHasJobs, clearAllCancelled, foldDrawCommands, isCancelledInAnyPlot, newPlot, runFirstJob, clearAddPlotCommands, setFirstRunningJob, updateExpressionPlotCommands)
import Components.Main.Types (ChildSlots, Config, State)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parSequence)
import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Draw.Commands (DrawCommand)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Halogen as H
import Halogen.Query.EventSource as ES
import Plot.Commands (clear, roughPlot)
import Plot.JobBatcher (JobResult, addPlot, cancelAll)
import Plot.Pan (panBounds, panBoundsByVector)
import Plot.PlotController (computePlotAsync)
import Plot.Zoom (zoomBounds)
import Types (Direction, XYBounds)
import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document, toEventTarget) as Web
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.WheelEvent as WE
import Web.UIEvent.WheelEvent.EventTypes as WET

type HalogenMain output a
  = H.HalogenM State Action ChildSlots output (ReaderT Config Aff) a

data Action
  = Init
  | Pan Direction
  | Zoom Boolean
  | HandleExpressionManager ExpressionManagerMessage
  | Scroll WheelEvent
  | HandleCanvas CanvasMessage
  | HandleBoundsInput BoundsInputMessage
  | HandleBatchInput BatchInputMessage
  | DrawPlot
  | ProcessNextJob
  | ResizeAndRedraw

handleAction :: forall output. Action -> HalogenMain output Unit
handleAction action = do
  state <- H.get
  case action of
    HandleExpressionManager message -> handleExpressionPlotMessage state message
    HandleBoundsInput (UpdatedBoundsInput newBounds) -> redrawWithBounds state newBounds
    Pan direction -> redrawWithDelayAndBounds state (panBounds state.bounds direction)
    Zoom isZoomIn -> redrawWithDelayAndBounds state (zoomBounds state.bounds isZoomIn)
    HandleCanvas message -> handleCanvasMessage state message
    Init -> initialiseAction state
    Scroll event -> scrollAction event
    DrawPlot -> H.modify_ (_ { input { operations = foldDrawCommands state } })
    ProcessNextJob -> processNextJobAction state
    HandleBatchInput (UpdatedBatchInput batchCount) -> do
      H.modify_ (_ { batchCount = batchCount })
      redraw state { batchCount = batchCount }
    ResizeAndRedraw -> do
      resizeCanvas
      newState <- H.get
      redraw newState

handleJobResult :: forall output. Maybe JobResult -> State -> HalogenMain output Unit
handleJobResult Nothing _ = pure unit

handleJobResult (Just jobResult) newState =
  if isCancelledInAnyPlot jobResult.job newState.plots then
    pure unit
  else do
    H.modify_ (_ { plots = alterPlot (updateExpressionPlotCommands jobResult.drawCommands) jobResult.job.batchId newState.plots })
    handleAction DrawPlot
    handleAction ProcessNextJob

scrollAction :: forall output. WheelEvent -> HalogenMain output Unit
scrollAction event = do
  when (changeInY /= 0.0) do
    H.liftEffect $ E.preventDefault (WE.toEvent event)
    handleAction $ Zoom (changeInY < 0.0)
  where
  changeInY = WE.deltaY event

resizeCanvas :: forall output. HalogenMain output Unit
resizeCanvas = do
  maybeNewCanvasSize <- H.liftEffect calculateNewCanvasSize
  case maybeNewCanvasSize of
    Nothing -> pure unit
    Just newCanvasSize -> do
      H.modify_ (_ { input { size = newCanvasSize } })

initialiseAction :: forall output. State -> HalogenMain output Unit
initialiseAction state = do
  window <- H.liftEffect $ Web.toEventTarget <$> Web.window
  document <- H.liftEffect $ Web.document =<< Web.window
  H.subscribe' \id -> ES.eventListenerEventSource WET.wheel (HTMLDocument.toEventTarget document) (map Scroll <<< WE.fromEvent)
  H.subscribe' \id -> ES.eventListenerEventSource (E.EventType "resize") window (const (Just ResizeAndRedraw))
  resizeCanvas
  clearAction state

clearAction :: forall output. State -> HalogenMain output Unit
clearAction state = do
  clearBounds <- lift $ lift $ computePlotAsync state.input.size (clear state.bounds)
  H.modify_ (_ { plots = [ newPlot 0 ], clearPlot = clearBounds })
  handleAction DrawPlot

processNextJobAction :: forall output. State -> HalogenMain output Unit
processNextJobAction state = do
  if (anyPlotHasJobs state.plots) then do
    H.modify_ (_ { plots = setFirstRunningJob state.plots })
    maybeJobResult <- lift $ lift $ runFirstJob state.input.size state.bounds.xBounds state.plots
    _ <- fork -- Subsiquent code is placed on the end of the JS event queue
    newState <- H.get
    handleJobResult maybeJobResult newState
  else
    H.modify_ (_ { plots = clearAllCancelled state.plots })

handleCanvasMessage :: forall output. State -> CanvasMessage -> HalogenMain output Unit
handleCanvasMessage state (Dragged delta) = redrawWithoutRobustWithBounds state (panBoundsByVector state.input.size state.bounds delta)

handleCanvasMessage state StoppedDragging = redraw state

handleExpressionPlotMessage :: forall output. State -> ExpressionManagerMessage -> HalogenMain output Unit
handleExpressionPlotMessage state (RaisedExpressionInputMessage (ParsedExpression id expression text)) = do
  newRoughCommands <- lift $ lift $ computePlotAsync state.input.size (roughPlot state.bounds expression text)
  H.modify_ (_ { plots = alterPlot (updatePlot newRoughCommands) id state.plots })
  handleAction DrawPlot
  handleAction ProcessNextJob
  where
  updatePlot :: DrawCommand Unit -> ExpressionPlot -> ExpressionPlot
  updatePlot newRoughCommands plot =
    plot
      { expressionText = text
      , expression = Just expression
      , roughDrawCommands = newRoughCommands
      , robustDrawCommands = pure unit
      , queue = queue
      }
    where
    queue =
      if state.autoRobust then
        addPlot plot.accuracy state.batchCount (cancelAll plot.queue) state.bounds expression text id
      else
        cancelAll plot.queue

handleExpressionPlotMessage state (RaisedExpressionInputMessage (ChangedStatus id status)) = do
  H.modify_ (_ { plots = alterPlot (_ { status = status }) id state.plots })
  handleAction DrawPlot

handleExpressionPlotMessage state (RaisedExpressionInputMessage (ParsedAccuracy id accuracy)) = do
  H.modify_ (_ { plots = alterPlot updatePlot id state.plots })
  handleAction DrawPlot
  handleAction ProcessNextJob
  where
  updatePlot :: ExpressionPlot -> ExpressionPlot
  updatePlot plot =
    plot
      { robustDrawCommands = pure unit
      , queue = queue
      , accuracy = accuracy
      }
    where
    queue = case plot.expression, state.autoRobust of
      Just expression, true -> addPlot accuracy state.batchCount (cancelAll plot.queue) state.bounds expression plot.expressionText plot.id
      _, _ -> cancelAll plot.queue

handleExpressionPlotMessage state (DeletePlot plotId) = do
  H.modify_ (_ { plots = filter (\p -> p.id /= plotId) state.plots })
  handleAction DrawPlot

handleExpressionPlotMessage state (ToggleAuto autoRobust) = H.modify_ (_ { autoRobust = autoRobust })

handleExpressionPlotMessage state (AddPlot nextId) = H.modify_ (_ { plots = state.plots <> [ newPlot nextId ] })

handleExpressionPlotMessage state (RenamePlot plotId name) = H.modify_ (_ { plots = alterPlot (_ { name = name }) plotId state.plots })

handleExpressionPlotMessage state ClearPlots = clearAction state

forkWithDelay :: forall output. Number -> HalogenMain output Unit
forkWithDelay duration = lift $ lift $ delay $ Milliseconds duration

fork :: forall output. HalogenMain output Unit
fork = forkWithDelay 0.0

redrawWithDelayAndBounds :: forall output. State -> XYBounds -> HalogenMain output Unit
redrawWithDelayAndBounds state newBounds = do
  plots <- lift $ lift $ clearAddPlotCommands state.autoRobust state.batchCount state.input.size newBounds state.plots
  clearBounds <- lift $ lift $ computePlotAsync state.input.size (clear newBounds)
  H.modify_ (_ { plots = plots, clearPlot = clearBounds, bounds = newBounds })
  handleAction DrawPlot
  _ <- forkWithDelay 500.0 -- Subsiquent code is placed on the end of the JS event queue
  handleAction ProcessNextJob

redraw :: forall output. State -> HalogenMain output Unit
redraw state = do
  plots <- lift $ lift $ clearAddPlotCommands state.autoRobust state.batchCount state.input.size state.bounds state.plots
  clearBounds <- lift $ lift $ computePlotAsync state.input.size (clear state.bounds)
  H.modify_ (_ { plots = plots, clearPlot = clearBounds })
  handleAction DrawPlot
  handleAction ProcessNextJob

redrawWithBounds :: forall output. State -> XYBounds -> HalogenMain output Unit
redrawWithBounds state newBounds = do
  H.modify_ (_ { bounds = newBounds })
  redraw state { bounds = newBounds }

redrawWithoutRobustWithBounds :: forall output. State -> XYBounds -> HalogenMain output Unit
redrawWithoutRobustWithBounds state newBounds = do
  plots <- mapPlots clearAddDrawRough state.plots
  clearBounds <- lift $ lift $ computePlotAsync state.input.size (clear newBounds)
  H.modify_ (_ { plots = plots, clearPlot = clearBounds, bounds = newBounds })
  handleAction DrawPlot
  where
  mapPlots :: (ExpressionPlot -> Aff ExpressionPlot) -> Array ExpressionPlot -> HalogenMain output (Array ExpressionPlot)
  mapPlots f = lift <<< lift <<< parSequence <<< (map f)

  clearAddDrawRough :: ExpressionPlot -> Aff ExpressionPlot
  clearAddDrawRough plot = case plot.expression of
    Just expression -> do
      drawCommands <- computePlotAsync state.input.size $ roughPlot newBounds expression plot.expressionText
      pure $ plot { queue = cancelAll plot.queue, roughDrawCommands = drawCommands, robustDrawCommands = pure unit }
    _ -> pure plot

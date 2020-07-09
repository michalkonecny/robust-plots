module Components.Main.Action where

import Prelude
import Components.BatchInput (BatchInputMessage(..))
import Components.BoundsInput (BoundsInputMessage(..))
import Components.Canvas (CanvasMessage(..), calculateNewCanvasSize)
import Components.ExpressionInput (ExpressionInputMessage(..), Status(..))
import Components.ExpressionManager (ExpressionManagerMessage(..))
import Components.ExpressionManager.Types (DrawingStatus(..), ExpressionPlot)
import Components.Main.Helper (alterPlot, anyPlotHasJobs, clearAddPlotCommands, clearAllCancelled, foldDrawCommands, fromPixelAccuracy, isCancelledInAnyPlot, newPlot, queueHasJobs, runFirstJob, setFirstRunningJob, updateExpressionPlotCommands)
import Components.Main.Types (ChildSlots, Config, State)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parSequence)
import Data.Array (filter)
import Data.Maybe (Maybe(..), isJust)
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
import Web.HTML.Window (document, toEventTarget) as Web

type HalogenMain output a
  = H.HalogenM State Action ChildSlots output (ReaderT Config Aff) a

data Action
  = Init
  | Pan Direction
  | Zoom Boolean
  | HandleExpressionManager ExpressionManagerMessage
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
  when (not (isCancelledInAnyPlot jobResult.job newState.plots)) do
    H.modify_ (_ { plots = alterPlot updatePlot jobResult.job.batchId newState.plots })
    handleAction DrawPlot
    handleAction ProcessNextJob
  where
  updatePlot :: ExpressionPlot -> ExpressionPlot
  updatePlot plot = updateExpressionPlotCommands jobResult.drawCommands $ plot { commands { status = status } }
    where
    status = if queueHasJobs plot then plot.commands.status else DrawnRobust

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

handleCanvasMessage state (Scrolled isZoomedIn) = handleAction $ Zoom isZoomedIn

handleExpressionPlotMessage :: forall output. State -> ExpressionManagerMessage -> HalogenMain output Unit
handleExpressionPlotMessage state (RaisedExpressionInputMessage (ParsedExpression id expression text)) = do
  newRoughCommands <- lift $ lift $ computePlotAsync state.input.size (roughPlot state.bounds expression text)
  H.modify_ (_ { plots = alterPlot (updatePlot newRoughCommands) id state.plots })
  handleAction DrawPlot
  handleAction ProcessNextJob
  where
  toDomainAccuracy :: Number -> Number
  toDomainAccuracy = fromPixelAccuracy state.input.size state.bounds

  updatePlot :: DrawCommand Unit -> ExpressionPlot -> ExpressionPlot
  updatePlot newRoughCommands plot =
    plot
      { expressionText = text
      , expression = Just expression
      , commands
        { rough = newRoughCommands
        , robust = pure unit
        , status = status
        }
      , queue = queue
      }
    where
    status = if state.autoRobust then RobustInProgress else DrawnRough

    queue =
      if state.autoRobust then
        addPlot (toDomainAccuracy plot.accuracy) state.batchCount (cancelAll plot.queue) state.bounds expression text id
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
  toDomainAccuracy :: Number -> Number
  toDomainAccuracy = fromPixelAccuracy state.input.size state.bounds

  updatePlot :: ExpressionPlot -> ExpressionPlot
  updatePlot plot =
    plot
      { commands
        { robust = pure unit }
      , queue = queue
      , accuracy = accuracy
      }
    where
    status = if plot.status == Robust && isJust plot.expression then RobustInProgress else DrawnRough

    queue = case plot.expression, plot.status == Robust of
      Just expression, true -> addPlot (toDomainAccuracy accuracy) state.batchCount (cancelAll plot.queue) state.bounds expression plot.expressionText plot.id
      _, _ -> cancelAll plot.queue

handleExpressionPlotMessage state (DeletePlot plotId) = do
  H.modify_ (_ { plots = filter (\p -> p.id /= plotId) state.plots })
  handleAction DrawPlot

handleExpressionPlotMessage state (ToggleAuto autoRobust) = H.modify_ (_ { autoRobust = autoRobust })

handleExpressionPlotMessage state (AddPlot nextId) = H.modify_ (_ { plots = state.plots <> [ newPlot nextId ] })

handleExpressionPlotMessage state (RenamePlot plotId name) = H.modify_ (_ { plots = alterPlot (_ { name = name }) plotId state.plots })

handleExpressionPlotMessage state ClearPlots = clearAction state

handleExpressionPlotMessage state CalulateRobustPlots = redrawRough state

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

redrawRough :: forall output. State -> HalogenMain output Unit
redrawRough state = do
  plots <- mapPlots clearAddPlot state.plots
  clearBounds <- lift $ lift $ computePlotAsync state.input.size (clear state.bounds)
  H.modify_ (_ { plots = plots, clearPlot = clearBounds })
  handleAction DrawPlot
  handleAction ProcessNextJob
  where
  toDomainAccuracy :: Number -> Number
  toDomainAccuracy = fromPixelAccuracy state.input.size state.bounds

  clearAddPlot :: ExpressionPlot -> Aff ExpressionPlot
  clearAddPlot plot = case plot.expression, plot.commands.status /= DrawnRobust && plot.status == Robust of
    Just expression, true -> do
      drawCommands <- computePlotAsync state.input.size $ roughPlot state.bounds expression plot.expressionText
      pure $ plot { queue = queue, commands { rough = drawCommands, robust = pure unit, status = RobustInProgress } }
      where
      queue = addPlot (toDomainAccuracy plot.accuracy) state.batchCount (cancelAll plot.queue) state.bounds expression plot.expressionText plot.id
    _, _ -> pure plot

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
  clearAddDrawRough :: ExpressionPlot -> Aff ExpressionPlot
  clearAddDrawRough plot = case plot.expression of
    Just expression -> do
      drawCommands <- computePlotAsync state.input.size $ roughPlot newBounds expression plot.expressionText
      pure $ plot { queue = cancelAll plot.queue, commands { rough = drawCommands, robust = pure unit, status = DrawnRough } }
    _ -> pure plot

mapPlots :: forall output. (ExpressionPlot -> Aff ExpressionPlot) -> Array ExpressionPlot -> HalogenMain output (Array ExpressionPlot)
mapPlots f = lift <<< lift <<< parSequence <<< (map f)

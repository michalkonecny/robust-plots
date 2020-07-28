module Components.Main.Action where

import Prelude
import Components.BatchInput (BatchInputMessage(..))
import Components.BoundsInput (BoundsInputMessage(..), canvasSizeToBounds)
import Components.Canvas (CanvasMessage(..), calculateNewCanvasSize)
import Components.ExpressionInput.FunctionExpressionInput (FunctionExpressionInputMessage(..))
import Components.ExpressionInput.ParametricExpressionInput (ParametricExpressionInputMessage(..))
import Components.ExpressionManager (ExpressionManagerMessage(..))
import Components.Main.Helper (foldDrawCommands)
import Components.Main.Types (ChildSlots, Config, State)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parSequence)
import Data.Array (filter, foldl, snoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Console (log)
import Effect.Exception (Error)
import Halogen as H
import Halogen.Query.EventSource as ES
import Plot.Commands (clear)
import Plot.JobBatcher (JobResult)
import Plot.Pan (panBounds, panBoundsByVector)
import Plot.PlotController (computePlotAsync)
import Plot.Zoom (zoomBounds)
import Types (Direction, XYBounds)
import ViewModels.Expression (ExpressionViewModel, newFunctionExpressionViewModel)
import ViewModels.Expression.Common (fromPixelAccuracy)
import ViewModels.Expression.Draw (appendRobustDrawCommands, drawRobustOnly, drawRoughAndRobust, drawRoughOnly, overwiteAccuracy)
import ViewModels.Expression.Generic (alterExpression, alterExpressionAsync, expressionId, overwriteName, overwriteStatus)
import ViewModels.Expression.Job (anyHasJobs, clearCancelledJobs, countBatches, isJobCancelled, runFirstJob, setFirstRunningJob)
import ViewModels.Expression.Unsafe (overwriteFunctionExpression, overwriteParametricExpression)
import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.Window (toEventTarget) as Web

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
    HandleBoundsInput message -> handleBoundsInputMessage state message
    Pan direction -> redrawWithDelayAndBounds state (panBounds state.bounds direction)
    Zoom isZoomIn -> redrawWithDelayAndBounds state (zoomBounds state.bounds isZoomIn)
    HandleCanvas message -> handleCanvasMessage state message
    Init -> initialiseAction
    DrawPlot -> H.modify_ (_ { input { operations = foldDrawCommands state } })
    ProcessNextJob -> processNextJobAction state
    HandleBatchInput (UpdatedBatchInput batchCount) -> do
      H.modify_ (_ { batchCount = batchCount })
      redraw state { batchCount = batchCount }
    ResizeAndRedraw -> do
      resizeCanvas
      newState <- H.get
      redraw newState

handleJobResult :: forall output. Maybe (Either Error JobResult) -> State -> HalogenMain output Unit
handleJobResult Nothing _ = pure unit

handleJobResult (Just jobResultOrError) newState =
  handleError jobResultOrError
    $ \jobResult -> do
        let
          updatePlot :: ExpressionViewModel -> ExpressionViewModel
          updatePlot plot = appendRobustDrawCommands jobResult.drawCommands $ plot
        when (not (isJobCancelled jobResult.job newState.plots)) do
          H.modify_ (_ { plots = alterExpression updatePlot jobResult.job.batchId newState.plots })
          updateProgress newState
          handleAction DrawPlot
          fork
          handleAction ProcessNextJob

resizeCanvas :: forall output. HalogenMain output Unit
resizeCanvas = do
  newCanvasSize <- H.liftEffect calculateNewCanvasSize
  H.modify_ (_ { input { size = newCanvasSize }, bounds = canvasSizeToBounds newCanvasSize })

initialiseAction :: forall output. HalogenMain output Unit
initialiseAction = do
  window <- H.liftEffect $ Web.toEventTarget <$> Web.window
  H.subscribe' \id -> ES.eventListenerEventSource (E.EventType "resize") window (const (Just ResizeAndRedraw))
  resizeCanvas
  state <- H.get
  clearAction state

handleBoundsInputMessage :: forall output. State -> BoundsInputMessage -> HalogenMain output Unit
handleBoundsInputMessage state (UpdatedBoundsInput newBounds) = redrawWithBounds state newBounds

handleBoundsInputMessage state ResetBounds = redrawWithBounds state $ canvasSizeToBounds state.input.size

clearAction :: forall output. State -> HalogenMain output Unit
clearAction state = do
  clearGlobalError
  clearBoundsOrError <- H.liftAff $ computePlotAsync state.input.size (clear state.bounds)
  handleError clearBoundsOrError
    $ \clearBounds -> do
        H.modify_ (_ { plots = [ newFunctionExpressionViewModel 0 ], clearPlot = clearBounds })
        handleAction DrawPlot

processNextJobAction :: forall output. State -> HalogenMain output Unit
processNextJobAction state = do
  if (anyHasJobs state.plots) then do
    H.modify_ (_ { plots = setFirstRunningJob state.plots })
    maybeJobResult <- H.liftAff $ runFirstJob state.input.size state.plots
    fork -- Subsiquent code is placed on the end of the JS event queue
    newState <- H.get
    handleJobResult maybeJobResult newState
  else do
    H.modify_ (_ { plots = clearCancelledJobs state.plots })
    updateProgress state

handleCanvasMessage :: forall output. State -> CanvasMessage -> HalogenMain output Unit
handleCanvasMessage state (Dragged delta) = redrawWithoutRobustWithBounds state (panBoundsByVector state.input.size state.bounds delta)

handleCanvasMessage state StoppedDragging = redraw state

handleCanvasMessage state (Scrolled isZoomedIn) = handleAction $ Zoom isZoomedIn

handleExpressionPlotMessage :: forall output. State -> ExpressionManagerMessage -> HalogenMain output Unit
handleExpressionPlotMessage state (RaisedFunctionExpressionInputMessage message) = handleFunctionExpressionPlotMessage state message

handleExpressionPlotMessage state (RaisedParametricExpressionInputMessage message) = handleParametricExpressionPlotMessage state message

handleExpressionPlotMessage state (DeletePlot plotId) = do
  H.modify_ (_ { plots = filter (\p -> plotId /= expressionId p) state.plots })
  handleAction DrawPlot

handleExpressionPlotMessage state (ToggleAuto autoRobust) = H.modify_ (_ { autoRobust = autoRobust })

handleExpressionPlotMessage state (AddPlot newPlot) = H.modify_ (_ { plots = state.plots <> [ newPlot ] })

handleExpressionPlotMessage state (RenamePlot plotId name) = do
  H.modify_ (_ { plots = alterExpression (overwriteName name) plotId state.plots })
  handleAction DrawPlot

handleExpressionPlotMessage state ClearPlots = clearAction state

handleExpressionPlotMessage state CalulateRobustPlots = redrawRobustOnly state

handleFunctionExpressionPlotMessage :: forall output. State -> FunctionExpressionInputMessage -> HalogenMain output Unit
handleFunctionExpressionPlotMessage state (FunctionParsedExpression id expression text) = do
  clearGlobalError
  plotsOrError <- H.liftAff $ alterExpressionAsync updatePlotWithExpression id state.plots
  handleError (toFirstError plotsOrError)
    $ \plots -> do
        H.modify_ (_ { plots = plots })
        resetProgress state { plots = plots }
        handleAction DrawPlot
        fork
        handleAction ProcessNextJob
  where
  updatePlotWithExpression = overwriteFunctionExpression expression text state.autoRobust state.batchCount state.input.size state.bounds

handleFunctionExpressionPlotMessage state (FunctionChangedStatus id status) = do
  H.modify_ (_ { plots = alterExpression (overwriteStatus status) id state.plots })
  handleAction DrawPlot

handleFunctionExpressionPlotMessage state (FunctionParsedAccuracy id accuracy) = do
  clearGlobalError
  H.modify_ (_ { inProgress = true })
  fork
  plotsOrError <- H.liftAff $ alterExpressionAsync (overwiteAccuracy accuracy toDomainAccuracy state.batchCount state.bounds) id state.plots
  handleError (toFirstError plotsOrError)
    $ \plots -> do
        H.modify_ (_ { plots = plots })
        resetProgress state { plots = plots }
        handleAction DrawPlot
        fork
        handleAction ProcessNextJob
  where
  toDomainAccuracy :: Number -> Number
  toDomainAccuracy = fromPixelAccuracy state.input.size state.bounds

handleParametricExpressionPlotMessage :: forall output. State -> ParametricExpressionInputMessage -> HalogenMain output Unit
handleParametricExpressionPlotMessage state (ParametricParsedExpression id expression text) = do
  clearGlobalError
  plotsOrError <- H.liftAff $ alterExpressionAsync updatePlotWithExpression id state.plots
  handleError (toFirstError plotsOrError)
    $ \plots -> do
        H.modify_ (_ { plots = plots })
        resetProgress state { plots = plots }
        handleAction DrawPlot
        fork
        handleAction ProcessNextJob
  where
  updatePlotWithExpression = overwriteParametricExpression expression text state.autoRobust state.batchCount state.input.size state.bounds

handleParametricExpressionPlotMessage state (ParametricChangedStatus id status) = do
  H.modify_ (_ { plots = alterExpression (overwriteStatus status) id state.plots })
  handleAction DrawPlot

handleParametricExpressionPlotMessage state (ParametricParsedAccuracy id accuracy) = do
  clearGlobalError
  H.modify_ (_ { inProgress = true })
  fork
  plotsOrError <- H.liftAff $ alterExpressionAsync (overwiteAccuracy accuracy toDomainAccuracy state.batchCount state.bounds) id state.plots
  handleError (toFirstError plotsOrError)
    $ \plots -> do
        H.modify_ (_ { plots = plots })
        resetProgress state { plots = plots }
        handleAction DrawPlot
        fork
        handleAction ProcessNextJob
  where
  toDomainAccuracy :: Number -> Number
  toDomainAccuracy = fromPixelAccuracy state.input.size state.bounds

forkWithDelay :: forall output. Number -> HalogenMain output Unit
forkWithDelay duration = H.liftAff $ delay $ Milliseconds duration

fork :: forall output. HalogenMain output Unit
fork = forkWithDelay 0.0

redrawWithDelayAndBounds :: forall output. State -> XYBounds -> HalogenMain output Unit
redrawWithDelayAndBounds state newBounds = do
  clearGlobalError
  if state.autoRobust then do
    clearBoundsOrError <- H.liftAff $ computePlotAsync state.input.size (clear newBounds)
    handleError clearBoundsOrError
      $ \clearBounds -> do
          H.modify_ (_ { clearPlot = clearBounds, inProgress = true, bounds = newBounds })
          handleAction DrawPlot
          fork
          plotsOrError <- mapPlots (drawRoughAndRobust state.autoRobust state.batchCount state.input.size newBounds) state.plots
          handleError (toFirstError plotsOrError)
            $ \plots -> do
                H.modify_ (_ { plots = plots })
                resetProgress state { plots = plots }
  else do
    clearBoundsOrError <- H.liftAff $ computePlotAsync state.input.size (clear newBounds)
    handleError clearBoundsOrError
      $ \clearBounds -> do
          plotsOrError <- mapPlots (drawRoughAndRobust state.autoRobust state.batchCount state.input.size newBounds) state.plots
          handleError (toFirstError plotsOrError)
            $ \plots -> do
                H.modify_ (_ { clearPlot = clearBounds, inProgress = true, bounds = newBounds, plots = plots })
                resetProgress state { plots = plots }
  handleAction DrawPlot
  forkWithDelay 500.0 -- Subsiquent code is placed on the end of the JS event queue
  handleAction ProcessNextJob

redraw :: forall output. State -> HalogenMain output Unit
redraw state = do
  clearGlobalError
  clearBoundsOrError <- H.liftAff $ computePlotAsync state.input.size (clear state.bounds)
  handleError clearBoundsOrError
    $ \clearBounds -> do
        H.modify_ (_ { clearPlot = clearBounds, inProgress = true })
        handleAction DrawPlot
        fork
        plotsOrError <- mapPlots (drawRoughAndRobust state.autoRobust state.batchCount state.input.size state.bounds) state.plots
        handleError (toFirstError plotsOrError)
          $ \plots -> do
              H.modify_ (_ { plots = plots })
              resetProgress state { plots = plots }
              handleAction DrawPlot
              fork
              handleAction ProcessNextJob

redrawRobustOnly :: forall output. State -> HalogenMain output Unit
redrawRobustOnly state = do
  clearGlobalError
  H.modify_ (_ { inProgress = true })
  handleAction DrawPlot
  fork
  plotsOrError <- mapPlots (drawRobustOnly state.batchCount state.input.size state.bounds) state.plots
  handleError (toFirstError plotsOrError)
    $ \plots -> do
        H.modify_ (_ { plots = plots })
        resetProgress state { plots = plots }
        handleAction DrawPlot
        fork
        handleAction ProcessNextJob

redrawWithBounds :: forall output. State -> XYBounds -> HalogenMain output Unit
redrawWithBounds state newBounds = do
  H.modify_ (_ { bounds = newBounds })
  redraw state { bounds = newBounds }

redrawWithoutRobustWithBounds :: forall output. State -> XYBounds -> HalogenMain output Unit
redrawWithoutRobustWithBounds state newBounds = do
  clearGlobalError
  plotsOrError <- mapPlots (drawRoughOnly state.input.size newBounds) state.plots
  handleError (toFirstError plotsOrError)
    $ \plots -> do
        clearBoundsOrError <- H.liftAff $ computePlotAsync state.input.size (clear newBounds)
        handleError clearBoundsOrError
          $ \clearBounds -> do
              H.modify_ (_ { plots = plots, clearPlot = clearBounds, bounds = newBounds })
              handleAction DrawPlot

mapPlots :: forall output. (ExpressionViewModel -> Aff (Either Error ExpressionViewModel)) -> Array ExpressionViewModel -> HalogenMain output (Array (Either Error ExpressionViewModel))
mapPlots f = lift <<< lift <<< parSequence <<< (map f)

resetProgress :: forall output. State -> HalogenMain output Unit
resetProgress state = H.modify_ (_ { progress = { index, total }, inProgress = index /= total })
  where
  index = 0

  total = countBatches state.plots

updateProgress :: forall output. State -> HalogenMain output Unit
updateProgress state = H.modify_ (_ { progress = { index, total }, inProgress = index /= total })
  where
  batchCount = countBatches state.plots

  index = state.progress.total - batchCount

  total = state.progress.total

toFirstError :: Array (Either Error ExpressionViewModel) -> Either Error (Array ExpressionViewModel)
toFirstError = foldl toSingleError (Right [])
  where
  toSingleError :: Either Error (Array ExpressionViewModel) -> Either Error ExpressionViewModel -> Either Error (Array ExpressionViewModel)
  toSingleError error@(Left _) _ = error

  toSingleError (Right _) (Left error) = Left error

  toSingleError (Right plots) (Right plot) = Right $ snoc plots plot

handleError :: forall output a. Either Error a -> (a -> HalogenMain output Unit) -> HalogenMain output Unit
handleError (Right v) onSuccess = onSuccess v

handleError (Left error) _ = do
  H.liftEffect $ log $ show error
  H.modify_ (_ { error = Just error, inProgress = false })

clearGlobalError :: forall output. HalogenMain output Unit
clearGlobalError = H.modify_ (_ { error = Nothing })

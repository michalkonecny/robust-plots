module Components.Main.Action where

import Prelude

import Components.BatchInput (BatchInputMessage(..))
import Components.BoundsInput (BoundsInputMessage(..), canvasSizeToBounds)
import Components.Canvas (CanvasMessage(..), calculateNewCanvasSize)
import Components.ExpressionInput (ExpressionInputMessage(..), Status(..))
import Components.ExpressionManager (ExpressionManagerMessage(..))
import Components.ExpressionManager.Types (DrawingStatus(..), ExpressionPlot)
import Components.Main.Helper (alterPlot, alterPlotAsync, anyPlotHasJobs, clearAllCancelled, countBatches, defaultPlotName, foldDrawCommands, isCancelledInAnyPlot, newPlot, queueHasJobs, runFirstJob, setFirstRunningJob, updateExpressionPlotCommands)
import Components.Main.Types (ChildSlots, Config, State)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parSequence)
import Data.Array (filter, foldl, snoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Console (log)
import Effect.Exception (Error)
import Halogen as H
import Halogen.Query.EventSource as ES
import IntervalArith.Misc (rationalToNumber)
import Plot.Commands (clear, roughPlot)
import Plot.JobBatcher (JobResult, addPlot, cancelAll)
import Plot.Pan (panBounds, panBoundsByVector)
import Plot.PlotController (computePlotAsync)
import Plot.Zoom (zoomBounds)
import Types (Direction, XYBounds, Size)
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
          updatePlot :: ExpressionPlot -> ExpressionPlot
          updatePlot plot = updateExpressionPlotCommands jobResult.drawCommands $ plot { commands { status = status } }
            where
            status = if queueHasJobs plot then plot.commands.status else DrawnRobust
        when (not (isCancelledInAnyPlot jobResult.job newState.plots)) do
          H.modify_ (_ { plots = alterPlot updatePlot jobResult.job.batchId newState.plots })
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
        H.modify_ (_ { plots = [ newPlot 0 ], clearPlot = clearBounds })
        handleAction DrawPlot

processNextJobAction :: forall output. State -> HalogenMain output Unit
processNextJobAction state = do
  if (anyPlotHasJobs state.plots) then do
    H.modify_ (_ { plots = setFirstRunningJob state.plots })
    maybeJobResult <- H.liftAff $ runFirstJob state.input.size state.bounds.xBounds state.plots
    fork -- Subsiquent code is placed on the end of the JS event queue
    newState <- H.get
    handleJobResult maybeJobResult newState
  else do
    H.modify_ (_ { plots = clearAllCancelled state.plots })
    updateProgress state

handleCanvasMessage :: forall output. State -> CanvasMessage -> HalogenMain output Unit
handleCanvasMessage state (Dragged delta) = redrawWithoutRobustWithBounds state (panBoundsByVector state.input.size state.bounds delta)

handleCanvasMessage state StoppedDragging = redraw state

handleCanvasMessage state (Scrolled isZoomedIn) = handleAction $ Zoom isZoomedIn

handleExpressionPlotMessage :: forall output. State -> ExpressionManagerMessage -> HalogenMain output Unit
handleExpressionPlotMessage state (RaisedExpressionInputMessage (ParsedExpression id expression text)) = do
  clearGlobalError
  plotsOrError <- H.liftAff $ alterPlotAsync updatePlot id state.plots
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

  updatePlot :: ExpressionPlot -> Aff (Either Error ExpressionPlot)
  updatePlot plot = do
    newRoughCommandsOrError <- computePlotAsync state.input.size (roughPlot state.bounds expression text)
    queueOrError <-
      if state.autoRobust then
        addPlot (toDomainAccuracy plot.accuracy) state.batchCount (cancelAll plot.queue) state.bounds expression text id
      else
        pure $ Right $ cancelAll plot.queue
    case newRoughCommandsOrError, queueOrError of
      Left error, _ -> pure $ Left error
      _, Left error -> pure $ Left error
      Right newRoughCommands, Right queue ->
        pure $ Right
          $ plot
              { expressionText = text
              , expression = Just expression
              , commands
                { rough = newRoughCommands
                , robust = pure unit
                , status = status
                }
              , queue = queue
              , name = name
              }
    where
    status = if state.autoRobust then RobustInProgress else DrawnRough

    name = if plot.name == defaultPlotName id || plot.name == plot.expressionText then text else plot.name

handleExpressionPlotMessage state (RaisedExpressionInputMessage (ChangedStatus id status)) = do
  H.modify_ (_ { plots = alterPlot (_ { status = status }) id state.plots })
  handleAction DrawPlot

handleExpressionPlotMessage state (RaisedExpressionInputMessage (ParsedAccuracy id accuracy)) = do
  clearGlobalError
  H.modify_ (_ { inProgress = true })
  fork
  plotsOrError <- H.liftAff $ alterPlotAsync updatePlot id state.plots
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

  updatePlot :: ExpressionPlot -> Aff (Either Error ExpressionPlot)
  updatePlot plot = do
    queueOrError <- case plot.expression, startRobust of
      Just expression, true -> addPlot (toDomainAccuracy accuracy) state.batchCount (cancelAll plot.queue) state.bounds expression plot.expressionText plot.id
      _, _ -> pure $ Right $ cancelAll plot.queue
    case queueOrError of
      Left error -> pure $ Left error
      Right queue ->
        pure $ Right
          $ plot
              { commands
                { robust = pure unit
                , status = status
                }
              , queue = queue
              , accuracy = accuracy
              }
    where
    startRobust = plot.status == Robust && (plot.commands.status == DrawnRobust || plot.commands.status == RobustInProgress)

    status = if startRobust && isJust plot.expression then RobustInProgress else DrawnRough

handleExpressionPlotMessage state (DeletePlot plotId) = do
  H.modify_ (_ { plots = filter (\p -> p.id /= plotId) state.plots })
  handleAction DrawPlot

handleExpressionPlotMessage state (ToggleAuto autoRobust) = H.modify_ (_ { autoRobust = autoRobust })

handleExpressionPlotMessage state (AddPlot nextId) = H.modify_ (_ { plots = state.plots <> [ newPlot nextId ] })

handleExpressionPlotMessage state (RenamePlot plotId name) = do
  H.modify_ (_ { plots = alterPlot (_ { name = name }) plotId state.plots })
  handleAction DrawPlot

handleExpressionPlotMessage state CalulateRobustPlots = redrawRough state

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
          plotsOrError <- H.liftAff $ clearAddPlotCommands state.autoRobust state.batchCount state.input.size newBounds state.plots
          handleError (toFirstError plotsOrError)
            $ \plots -> do
                H.modify_ (_ { plots = plots })
                resetProgress state { plots = plots }
  else do
    clearBoundsOrError <- H.liftAff $ computePlotAsync state.input.size (clear newBounds)
    handleError clearBoundsOrError
      $ \clearBounds -> do
          plotsOrError <- H.liftAff $ clearAddPlotCommands state.autoRobust state.batchCount state.input.size newBounds state.plots
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
        plotsOrError <- H.liftAff $ clearAddPlotCommands state.autoRobust state.batchCount state.input.size state.bounds state.plots
        handleError (toFirstError plotsOrError)
          $ \plots -> do
              H.modify_ (_ { plots = plots })
              resetProgress state { plots = plots }
              handleAction DrawPlot
              fork
              handleAction ProcessNextJob

redrawRough :: forall output. State -> HalogenMain output Unit
redrawRough state = do
  clearGlobalError
  clearBoundsOrError <- H.liftAff $ computePlotAsync state.input.size (clear state.bounds)
  handleError clearBoundsOrError
    $ \clearBounds -> do
        H.modify_ (_ { clearPlot = clearBounds, inProgress = true })
        handleAction DrawPlot
        fork
        plotsOrError <- mapPlots clearAddPlot state.plots
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

  clearAddPlot :: ExpressionPlot -> Aff (Either Error ExpressionPlot)
  clearAddPlot plot = case plot.expression, plot.commands.status /= DrawnRobust && plot.status == Robust of
    Just expression, true -> do
      newRoughCommandsOrError <- computePlotAsync state.input.size $ roughPlot state.bounds expression plot.expressionText
      queueOrError <- addPlot (toDomainAccuracy plot.accuracy) state.batchCount (cancelAll plot.queue) state.bounds expression plot.expressionText plot.id
      case newRoughCommandsOrError, queueOrError of
        Left error, _ -> pure $ Left error
        _, Left error -> pure $ Left error
        Right drawCommands, Right queue -> pure $ Right $ plot { queue = queue, commands { rough = drawCommands, robust = pure unit, status = RobustInProgress } }
    _, _ -> pure $ Right plot

redrawWithBounds :: forall output. State -> XYBounds -> HalogenMain output Unit
redrawWithBounds state newBounds = do
  H.modify_ (_ { bounds = newBounds })
  redraw state { bounds = newBounds }

redrawWithoutRobustWithBounds :: forall output. State -> XYBounds -> HalogenMain output Unit
redrawWithoutRobustWithBounds state newBounds = do
  clearGlobalError
  plotsOrError <- mapPlots clearAddDrawRough state.plots
  handleError (toFirstError plotsOrError)
    $ \plots -> do
        clearBoundsOrError <- H.liftAff $ computePlotAsync state.input.size (clear newBounds)
        handleError clearBoundsOrError
          $ \clearBounds -> do
              H.modify_ (_ { plots = plots, clearPlot = clearBounds, bounds = newBounds })
              handleAction DrawPlot
  where
  clearAddDrawRough :: ExpressionPlot -> Aff (Either Error ExpressionPlot)
  clearAddDrawRough plot = case plot.expression of
    Just expression -> do
      drawCommandsOrError <- computePlotAsync state.input.size $ roughPlot newBounds expression plot.expressionText
      case drawCommandsOrError of
        (Left error) -> pure $ Left error
        Right drawCommands -> pure $ Right $ plot { queue = cancelAll plot.queue, commands { rough = drawCommands, robust = pure unit, status = DrawnRough } }
    _ -> pure $ Right plot

mapPlots :: forall output. (ExpressionPlot -> Aff (Either Error ExpressionPlot)) -> Array ExpressionPlot -> HalogenMain output (Array (Either Error ExpressionPlot))
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

clearAddPlotCommands :: Boolean -> Int -> Size -> XYBounds -> Array ExpressionPlot -> Aff (Array (Either Error ExpressionPlot))
clearAddPlotCommands autoRobust batchCount size newBounds = parSequence <<< (map clearAddPlot)
  where
  toDomainAccuracy :: Number -> Number
  toDomainAccuracy = fromPixelAccuracy size newBounds

  clearAddPlot :: ExpressionPlot -> Aff (Either Error ExpressionPlot)
  clearAddPlot plot = case plot.expression of
    Nothing -> pure $ Right plot
    Just expression -> do
      drawCommandsOrError <- computePlotAsync size $ roughPlot newBounds expression plot.expressionText
      queueOrError <-
        if autoRobust && plot.status == Robust then
          addPlot (toDomainAccuracy plot.accuracy) batchCount (cancelAll plot.queue) newBounds expression plot.expressionText plot.id
        else
          pure $ Right $ cancelAll plot.queue
      case drawCommandsOrError, queueOrError of
        Left error, _ -> pure $ Left error
        _, Left error -> pure $ Left error
        Right drawCommands, Right queue -> pure $ Right $ plot { queue = queue, commands { rough = drawCommands, robust = pure unit, status = status } }
      where
      status =
        if autoRobust && plot.status == Robust then
          RobustInProgress
        else
          DrawnRough

fromPixelAccuracy :: Size -> XYBounds -> Number -> Number
fromPixelAccuracy canvasSize bounds pixelAccuracy = pixelAccuracy * pixelToDomainRatio
  where
  rangeY = bounds.yBounds.upper - bounds.yBounds.lower

  pixelToDomainRatio = rationalToNumber $ rangeY / canvasSize.height

toFirstError :: Array (Either Error ExpressionPlot) -> Either Error (Array ExpressionPlot)
toFirstError = foldl toSingleError (Right [])
  where
  toSingleError :: Either Error (Array ExpressionPlot) -> Either Error ExpressionPlot -> Either Error (Array ExpressionPlot)
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
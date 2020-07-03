module Components.Main.Action where

import Prelude
import Components.AccuracyInput (AccuracyInputMessage(..))
import Components.BatchInput (BatchInputMessage(..))
import Components.BoundsInput (BoundsInputMessage(..))
import Components.Canvas (CanvasMessage(..))
import Components.ExpressionInput (ExpressionInputMessage(..))
import Components.Main.Helper (alterPlot, anyPlotHasJobs, clearAllCancelled, foldDrawCommands, initialBounds, isCancelledInAnyPlot, newPlot, runFirstJob, clearAddPlotCommands, setFirstRunningJob, updateExpressionPlotCommands)
import Components.Main.Types (ChildSlots, Config, State, ExpressionPlot)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parSequence)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Query.EventSource as ES
import IntervalArith.Misc (toRational)
import Plot.Commands (clear, roughPlot)
import Plot.JobBatcher (JobResult, addPlot, cancelAll)
import Plot.Pan (panBounds, panBoundsByVector)
import Plot.PlotController (computePlotAsync)
import Plot.Zoom (zoomBounds)
import Types (Direction, XYBounds, Size)
import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document, toEventTarget, innerWidth, innerHeight) as Web
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.WheelEvent as WE
import Web.UIEvent.WheelEvent.EventTypes as WET

type HalogenMain output a
  = H.HalogenM State Action ChildSlots output (ReaderT Config Aff) a

data Action
  = Clear
  | Init
  | Pan Direction
  | Zoom Boolean
  | HandleExpressionInput ExpressionInputMessage
  | HandleScroll WheelEvent
  | HandleCanvas CanvasMessage
  | HandleBoundsInput BoundsInputMessage
  | HandleBatchInput BatchInputMessage
  | HandleAccuracyInput AccuracyInputMessage
  | AddPlot
  | ResetBounds
  | DrawPlot
  | HandleQueue
  | HandleResize
  | ChangeSelectedPlot Int

handleAction :: forall output. Action -> HalogenMain output Unit
handleAction action = do
  state <- H.get
  case action of
    Clear -> do
      clearBounds <- lift $ lift $ computePlotAsync state.input.size (clear state.bounds)
      H.modify_ (_ { plots = [ newPlot 0 ], clearPlot = clearBounds, selectedPlot = 0 })
      handleAction DrawPlot
    HandleExpressionInput (Parsed id expression text) -> do
      newRoughCommands <- lift $ lift $ computePlotAsync state.input.size (roughPlot state.bounds expression text)
      let
        updatePlot :: ExpressionPlot -> ExpressionPlot
        updatePlot plot =
          plot
            { expressionText = text
            , expression = Just expression
            , roughDrawCommands = newRoughCommands
            , robustDrawCommands = pure unit
            , queue = addPlot state.accuracy state.batchCount (cancelAll plot.queue) state.bounds expression text id
            }
      H.modify_ (_ { plots = alterPlot updatePlot id state.plots })
      handleAction HandleQueue
    HandleExpressionInput (ChangedStatus id status) -> do
      H.modify_ (_ { plots = alterPlot (_ { status = status }) id state.plots })
      handleAction DrawPlot
    Pan direction -> redrawWithDelayAndBounds state (panBounds state.bounds direction)
    Zoom isZoomIn -> redrawWithDelayAndBounds state (zoomBounds state.bounds isZoomIn)
    ResetBounds -> redrawWithBounds state initialBounds
    HandleCanvas (Dragged delta) -> redrawWithoutRobustWithBounds state (panBoundsByVector state.input.size state.bounds delta)
    HandleCanvas StoppedDragging -> redraw state
    AddPlot -> H.modify_ (_ { plots = state.plots <> [ newPlot (length state.plots) ] })
    HandleBoundsInput (UpdatedBoundsInput newBounds) -> redrawWithBounds state newBounds
    Init -> do
      window <- H.liftEffect $ Web.toEventTarget <$> Web.window
      document <- H.liftEffect $ Web.document =<< Web.window
      H.subscribe' \id -> ES.eventListenerEventSource WET.wheel (HTMLDocument.toEventTarget document) (map HandleScroll <<< WE.fromEvent)
      H.subscribe' \id -> ES.eventListenerEventSource (E.EventType "resize") window (const (Just HandleResize))
      handleAction Clear
    HandleScroll event -> do
      let
        changeInY = WE.deltaY event
      when (changeInY /= 0.0) do
        H.liftEffect $ E.preventDefault (WE.toEvent event)
        handleAction $ Zoom (changeInY < 0.0)
    DrawPlot -> H.modify_ (_ { input { operations = foldDrawCommands state } })
    HandleQueue -> do
      if (anyPlotHasJobs state.plots) then do
        H.modify_ (_ { plots = setFirstRunningJob state.plots })
        maybeJobResult <- lift $ lift $ runFirstJob state.input.size state.bounds.xBounds state.plots
        _ <- fork -- Subsiquent code is placed on the end of the JS event queue
        newState <- H.get
        handleJobResult maybeJobResult newState
      else
        H.modify_ (_ { plots = clearAllCancelled state.plots })
    HandleBatchInput (UpdatedBatchInput batchCount) -> do
      H.modify_ (_ { batchCount = batchCount })
      redraw state { batchCount = batchCount }
    HandleAccuracyInput (UpdatedAccuracyInput accuracy) -> do
      H.modify_ (_ { accuracy = accuracy })
      redraw state { accuracy = accuracy }
    ChangeSelectedPlot plotId -> H.modify_ (_ { selectedPlot = plotId })
    HandleResize -> do
      windowSize <- getWindowSize
      H.liftEffect $ log "Resized"
      pure unit

getWindowSize :: forall output. HalogenMain output Size
getWindowSize = do
  window <- H.liftEffect $ Web.window
  width <- H.liftEffect $ Web.innerWidth window
  height <- H.liftEffect $ Web.innerHeight window
  pure $ { width: toRational width, height: toRational height }

handleJobResult :: forall output. Maybe JobResult -> State -> HalogenMain output Unit
handleJobResult Nothing _ = pure unit

handleJobResult (Just jobResult) newState =
  if isCancelledInAnyPlot jobResult.job newState.plots then
    pure unit
  else do
    H.modify_ (_ { plots = alterPlot (updateExpressionPlotCommands jobResult.drawCommands) jobResult.job.batchId newState.plots })
    handleAction DrawPlot
    handleAction HandleQueue

forkWithDelay :: forall output. Number -> HalogenMain output Unit
forkWithDelay duration = lift $ lift $ delay $ Milliseconds duration

fork :: forall output. HalogenMain output Unit
fork = forkWithDelay 0.0

redrawWithDelayAndBounds :: forall output. State -> XYBounds -> HalogenMain output Unit
redrawWithDelayAndBounds state newBounds = do
  plots <- lift $ lift $ clearAddPlotCommands state.accuracy state.batchCount state.input.size newBounds state.plots
  clearBounds <- lift $ lift $ computePlotAsync state.input.size (clear newBounds)
  H.modify_ (_ { plots = plots, clearPlot = clearBounds, bounds = newBounds })
  handleAction DrawPlot
  _ <- forkWithDelay 500.0 -- Subsiquent code is placed on the end of the JS event queue
  handleAction HandleQueue

redraw :: forall output. State -> HalogenMain output Unit
redraw state = do
  plots <- lift $ lift $ clearAddPlotCommands state.accuracy state.batchCount state.input.size state.bounds state.plots
  clearBounds <- lift $ lift $ computePlotAsync state.input.size (clear state.bounds)
  H.modify_ (_ { plots = plots, clearPlot = clearBounds })
  handleAction DrawPlot
  handleAction HandleQueue

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
    Nothing -> pure plot
    Just expression -> do
      drawCommands <- computePlotAsync state.input.size $ roughPlot newBounds expression plot.expressionText
      pure $ plot { queue = cancelAll plot.queue, roughDrawCommands = drawCommands, robustDrawCommands = pure unit }

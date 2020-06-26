module Components.Main.Action where

import Prelude
import Components.BatchInput (BatchInputMessage(..))
import Components.BoundsInput (BoundsInputMessage(..))
import Components.Canvas (CanvasMessage(..))
import Components.ExpressionInput (ExpressionInputMessage(..))
import Components.Main.Helper (alterPlot, anyPlotHasJobs, clearAllCancelled, foldDrawCommands, initialBounds, isCancelledInAnyPlot, newPlot, runFirstJob, setFirstRunningJob, updateExpressionPlotCommands)
import Components.Main.Types (ChildSlots, Config, State, ExpressionPlot)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parSequence)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Halogen as H
import Halogen.Query.EventSource as ES
import Plot.Commands (clear, robustPlot, roughPlot)
import Plot.JobBatcher (JobResult, addPlot, cancelAll)
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
      H.modify_ (_ { plots = [ newPlot 1 ], clearPlot = clearBounds })
      handleAction DrawPlot
    HandleExpressionInput (Parsed id expression text) -> do
      newRoughCommands <- lift $ lift $ computePlotAsync state.input.size (roughPlot state.bounds expression text)
      let
        robust = robustPlot state.segmentCount state.bounds expression text

        updatePlot :: ExpressionPlot -> ExpressionPlot
        updatePlot plot =
          plot
            { expressionText = text
            , expression = Just expression
            , roughDrawCommands = newRoughCommands
            , robustDrawCommands = pure unit
            , queue = addPlot state.batchCount (cancelAll plot.queue) robust id
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
      if (anyPlotHasJobs state.plots) then do
        H.modify_ (_ { plots = setFirstRunningJob state.plots })
        maybeJobResult <- lift $ lift $ runFirstJob state.input.size state.bounds.xBounds state.plots
        _ <- fork -- Subsiquent code is placed on the end of the JS event queue
        newState <- H.get
        handleJobResult maybeJobResult newState
      else
        H.modify_ (_ { plots = clearAllCancelled state.plots })
    HandleBatchInput (UpdatedBatchInput batchCount segmentCount) -> do
      H.modify_ (_ { batchCount = batchCount, segmentCount = segmentCount })
      redraw state { batchCount = batchCount, segmentCount = segmentCount }

handleJobResult :: forall output. Maybe JobResult -> State -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
handleJobResult Nothing _ = pure unit

handleJobResult (Just jobResult) newState =
  if isCancelledInAnyPlot jobResult.job newState.plots then
    pure unit
  else do
    H.modify_ (_ { plots = alterPlot (updateExpressionPlotCommands jobResult.drawCommands) jobResult.job.batchId newState.plots })
    handleAction DrawPlot
    handleAction HandleQueue

clearAddPlotCommands :: Int -> Int -> Size -> XYBounds -> Array ExpressionPlot -> Aff (Array ExpressionPlot)
clearAddPlotCommands batchCount segmentCount size newBounds = parSequence <<< (map clearAddPlot)
  where
  clearAddPlot :: ExpressionPlot -> Aff ExpressionPlot
  clearAddPlot plot = case plot.expression of
    Nothing -> pure plot
    Just expression -> do
      let
        robust = robustPlot segmentCount newBounds expression plot.expressionText

        cancelledQueue = cancelAll plot.queue

        queueWithPlot = addPlot batchCount cancelledQueue robust plot.id
      drawCommands <- computePlotAsync size $ roughPlot newBounds expression plot.expressionText
      pure $ plot { queue = queueWithPlot, roughDrawCommands = drawCommands, robustDrawCommands = pure unit }

forkWithDelay :: forall output. Number -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
forkWithDelay duration = lift $ lift $ delay $ Milliseconds duration

fork :: forall output. H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
fork = forkWithDelay 0.0

redrawWithDelayAndBounds :: forall output. State -> XYBounds -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
redrawWithDelayAndBounds state newBounds = do
  plots <- lift $ lift $ clearAddPlotCommands state.batchCount state.segmentCount state.input.size newBounds state.plots
  clearBounds <- lift $ lift $ computePlotAsync state.input.size (clear newBounds)
  H.modify_ (_ { plots = plots, clearPlot = clearBounds, bounds = newBounds })
  handleAction DrawPlot
  _ <- forkWithDelay 500.0 -- Subsiquent code is placed on the end of the JS event queue
  handleAction HandleQueue

redraw :: forall output. State -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
redraw state = do
  plots <- lift $ lift $ clearAddPlotCommands state.batchCount state.segmentCount state.input.size state.bounds state.plots
  clearBounds <- lift $ lift $ computePlotAsync state.input.size (clear state.bounds)
  H.modify_ (_ { plots = plots, clearPlot = clearBounds })
  handleAction DrawPlot
  handleAction HandleQueue

redrawWithBounds :: forall output. State -> XYBounds -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
redrawWithBounds state newBounds = do
  H.modify_ (_ { bounds = newBounds })
  redraw state { bounds = newBounds }

redrawWithoutRobustWithBounds :: forall output. State -> XYBounds -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
redrawWithoutRobustWithBounds state newBounds = do
  plots <- lift $ lift $ clearAddPlotCommands state.batchCount state.segmentCount state.input.size newBounds state.plots
  clearBounds <- lift $ lift $ computePlotAsync state.input.size (clear newBounds)
  H.modify_ (_ { plots = plots, clearPlot = clearBounds, bounds = newBounds })
  handleAction DrawPlot
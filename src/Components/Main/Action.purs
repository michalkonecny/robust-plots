module Components.Main.Action where

import Prelude (Unit, bind, discard, map, pure, unit, when, ($), (+), (/=), (<), (<<<), (<>), (=<<))
import Components.BoundsInput (BoundsInputMessage(..))
import Components.Canvas (CanvasMessage(..))
import Components.ExpressionInput (ExpressionInputMessage(..))
import Components.Main.Types (ChildSlots, Config, State)
import Components.Main.Helper (alterPlot, foldDrawCommands, initialBounds, newPlot, robustWithBounds, toMaybePlotCommandWithId, updateExpressionPlotCommands, updateExpressionPlotInfo)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array (length, mapMaybe)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Query.EventSource as ES
import Plot.Commands (clear, roughPlot)
import Plot.JobBatcher (JobResult, addManyPlots, addPlot, cancelAll, cancelWithBatchId, hasJobs, isCancelled, setRunning, runFirst, showQueueIds)
import Plot.Pan (panBounds, panBoundsByVector)
import Plot.PlotController (computePlotAsync)
import Plot.Zoom (zoomBounds)
import Types (Direction, Id, XYBounds)
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
      clearBounds <- lift $ lift $ computePlotAsync state.input.size (clear state.bounds)
      H.modify_ (_ { plots = [ newPlot 1 ], clearPlot = clearBounds, queue = cancelAll state.queue })
      handleAction DrawPlot
    HandleExpressionInput (Parsed id expression text) -> do
      let
        plots = alterPlot (updateExpressionPlotInfo expression text) id state.plots

        updatedQueue = cancelWithBatchId state.queue id

        rough = roughPlot state.bounds expression text

        robust = robustWithBounds state.bounds expression text

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
        H.modify_ (_ { queue = setRunning state.queue })
        maybeJobResult <- lift $ lift $ runFirst state.input.size state.bounds.xBounds state.queue
        _ <- fork
        newState <- H.get
        handleJobResult maybeJobResult newState

handleJobResult :: forall output. Maybe JobResult -> State -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
handleJobResult Nothing _ = pure unit

handleJobResult (Just jobResult) newState =
  if isCancelled newState.queue jobResult.job.id then
    pure unit
  else do
    H.modify_ (_ { plots = alterPlot (updateExpressionPlotCommands jobResult.drawCommands) jobResult.job.batchId newState.plots })
    handleAction DrawPlot
    handleAction HandleQueue

fork :: forall output. H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
fork = lift $ lift $ delay $ Milliseconds 0.0 -- TODO Remove artifical delay

redrawWithBounds :: forall output. State -> XYBounds -> H.HalogenM State Action ChildSlots output (ReaderT Config Aff) Unit
redrawWithBounds state newBounds = do
  let
    canceledQueue = cancelAll state.queue

    plots = map (_ { drawCommands = pure unit }) state.plots

    roughPlotsWithId = mapMaybe (toMaybePlotCommandWithId newBounds roughPlot) plots

    robustPlotsWithId = mapMaybe (toMaybePlotCommandWithId newBounds robustWithBounds) plots

    withRough = addManyPlots canceledQueue roughPlotsWithId

    withRobust = addManyPlots withRough robustPlotsWithId
  clearBounds <- lift $ lift $ computePlotAsync state.input.size (clear newBounds)
  H.modify_ (_ { plots = plots, queue = withRobust, clearPlot = clearBounds, bounds = newBounds })
  handleAction DrawPlot
  handleAction HandleQueue

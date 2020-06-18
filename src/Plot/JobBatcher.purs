module Plot.JobBatcher
  ( JobQueue
  , Job
  , JobResult
  , cancelAll
  , addPlot
  , initialJobQueue
  , cancelWithBatchId
  , addManyPlots
  , hasJobs
  , runFirst
  , showQueueIds
  , pop
  , isCancelled
  ) where

import Prelude
import Data.Array (elem, foldl, tail, zipWith, (..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Draw.Commands (DrawCommand)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Expression.Syntax (Expression)
import IntervalArith.Misc (Rational, toRational)
import Plot.Commands (PlotCommand(..), robustPlot)
import Plot.PlotController (computePlotAsync)
import Plot.Queue (Queue, empty, filter, mapToArray, null, peek, push, showWith, queueTail)
import Types (Id, Size, XYBounds, Bounds)

batchSegmentCount :: Int
batchSegmentCount = 5

type JobQueue
  = { cancelled :: Array Id
    , queue :: Queue Job
    , currentId :: Id
    }

type Job
  = { id :: Id
    , command :: PlotCommand
    , batchId :: Id
    }

type JobResult
  = { job :: Job
    , drawCommands :: DrawCommand Unit
    }

initialJobQueue :: JobQueue
initialJobQueue =
  { cancelled: []
  , currentId: 0
  , queue: empty
  }

hasJobs :: JobQueue -> Boolean
hasJobs jobQueue = not $ null jobQueue.queue

cancelAll :: JobQueue -> JobQueue
cancelAll jobQueue = jobQueue { cancelled = cancelled, queue = empty }
  where
  cancelled = jobQueue.cancelled <> mapToArray (\job -> job.id) jobQueue.queue

cancelWithBatchId :: JobQueue -> Id -> JobQueue
cancelWithBatchId jobQueue batchId = jobQueue { cancelled = cancelledIds, queue = active }
  where
  active = filter (\job -> job.batchId /= batchId) jobQueue.queue

  canceledJobs = (filter (\job -> job.batchId == batchId) jobQueue.queue)

  cancelledIds = jobQueue.cancelled <> (mapToArray (\job -> job.id) canceledJobs)

addManyPlots :: JobQueue -> Array (Tuple PlotCommand Id) -> JobQueue
addManyPlots jobQueue plots = foldl foldIntoJob jobQueue plots
  where
  foldIntoJob :: JobQueue -> Tuple PlotCommand Id -> JobQueue
  foldIntoJob queue (Tuple command batchId) = addPlot queue command batchId

addPlot :: JobQueue -> PlotCommand -> Id -> JobQueue
addPlot jobQueue p@(RobustPlot bounds expression label) batchId = foldl (addJob batchId) jobQueue segmentedPlots
  where
  segmentedPlots = segmentRobust bounds expression label

-- Add Rough and Empty plot as single jobs
addPlot jobQueue p batchId = addJob batchId jobQueue p

runFirst :: Size -> Bounds -> JobQueue -> Aff (Maybe JobResult)
runFirst canvasSize bounds jobQueue = runMaybeJob (runJob canvasSize bounds) jobQueue.cancelled maybeJob
  where
  maybeJob = peek jobQueue.queue

runMaybeJob :: (Job -> Aff (DrawCommand Unit)) -> Array Id -> Maybe Job -> Aff (Maybe JobResult)
runMaybeJob _ _ Nothing = pure Nothing

runMaybeJob runner cancelled (Just job) =
  if elem job.id cancelled then
    pure Nothing
  else do
    drawCommands <- runner job
    pure $ Just { job, drawCommands }

nullJob :: Aff (DrawCommand Unit)
nullJob = (liftAff <<< pure <<< pure) unit

runJob :: Size -> Bounds -> Job -> Aff (DrawCommand Unit)
runJob canvasSize bounds job = computePlotAsync canvasSize bounds job.command

segmentRobust :: XYBounds -> Expression -> String -> Array PlotCommand
segmentRobust bounds expression label = commands
  where
  rangeX = bounds.xBounds.upper - bounds.xBounds.lower

  rangeY = bounds.yBounds.upper - bounds.yBounds.lower

  segmentWidth = rangeX / (toRational batchSegmentCount)

  batchBreakpoints = map (toRational >>> toDomainX) $ 0 .. batchSegmentCount

  commands = zipWith toPlotCommand batchBreakpoints (fromMaybe [] (tail batchBreakpoints))

  toDomainX :: Rational -> Rational
  toDomainX segmentX = (segmentX * segmentWidth) + bounds.xBounds.lower

  toPlotCommand :: Rational -> Rational -> PlotCommand
  toPlotCommand lower upper = robustPlot commandBounds expression label
    where
    commandBounds = bounds { xBounds = { lower, upper } }

addJob :: Id -> JobQueue -> PlotCommand -> JobQueue
addJob batchId jobQueue command = jobQueue { queue = newQueue, currentId = newCurrentId }
  where
  newCurrentId = jobQueue.currentId + 1

  newQueue = push jobQueue.queue { id: newCurrentId, command, batchId }

pop :: JobQueue -> JobQueue
pop jobQueue = jobQueue { queue = queueTail jobQueue.queue }

isCancelled :: JobQueue -> Id -> Boolean
isCancelled jobQueue id = elem id jobQueue.cancelled

eqMaybeJob :: Maybe Job -> Maybe Job -> Boolean
eqMaybeJob Nothing Nothing = true

eqMaybeJob (Just jobA) (Just jobB) = jobA.id == jobB.id

eqMaybeJob _ _ = false

showQueueIds :: JobQueue -> String
showQueueIds jobQueue = showWith (\job -> show job.id) jobQueue.queue

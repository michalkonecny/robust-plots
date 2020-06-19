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
  , setRunning
  , isCancelled
  , clearCancelled
  ) where

import Prelude
import Data.Array (elem, foldl, foldr, tail, zipWith, (..))
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set, empty, insert) as S
import Data.Tuple (Tuple(..))
import Draw.Commands (DrawCommand)
import Effect.Aff (Aff)
import Effect.Exception.Unsafe (unsafeThrow)
import Expression.Syntax (Expression)
import IntervalArith.Misc (Rational, toRational)
import Plot.Commands (PlotCommand(..))
import Plot.PlotController (computePlotAsync)
import Plot.Queue (Queue, toList, empty, filter, null, peek, push, tail) as Q
import Types (Id, Size, XYBounds, Bounds)

type JobQueue
  = { cancelled :: S.Set Id
    , queue :: Q.Queue Job
    , currentId :: Id
    , running :: Maybe Job
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
  { cancelled: S.empty
  , currentId: 0
  , queue: Q.empty
  , running: Nothing
  }

hasJobs :: JobQueue -> Boolean
hasJobs jobQueue = not $ Q.null jobQueue.queue

clearCancelled :: JobQueue -> JobQueue
clearCancelled = _ { cancelled = S.empty }

cancelAll :: JobQueue -> JobQueue
cancelAll jobQueue = cancelRunning $ jobQueue { cancelled = cancelled, queue = Q.empty }
  where
  cancelled = insertAll (_.id) (Q.toList jobQueue.queue) jobQueue.cancelled

cancelWithBatchId :: JobQueue -> Id -> JobQueue
cancelWithBatchId jobQueue batchId = newQueue
  where
  hasBatchId :: Job -> Boolean
  hasBatchId job = job.batchId == batchId

  active = Q.filter (not <<< hasBatchId) jobQueue.queue

  cancelledJobs = Q.toList $ Q.filter hasBatchId jobQueue.queue

  cancelledActive = jobQueue { cancelled = insertAll (_.id) cancelledJobs jobQueue.cancelled, queue = active }

  newQueue = if isRunning hasBatchId jobQueue then cancelRunning $ cancelledActive else cancelledActive

addManyPlots :: JobQueue -> Array (Tuple PlotCommand Id) -> JobQueue
addManyPlots jobQueue plots = foldl foldIntoJob jobQueue plots
  where
  foldIntoJob :: JobQueue -> Tuple PlotCommand Id -> JobQueue
  foldIntoJob queue (Tuple command batchId) = addPlot queue command batchId

addPlot :: JobQueue -> PlotCommand -> Id -> JobQueue
addPlot jobQueue p@(RobustPlot bounds fullXBounds expression label) batchId = foldl (addJob batchId) jobQueue segmentedPlots
  where
  segmentedPlots = segmentRobust fullXBounds bounds expression label

-- Rough and Empty plot should be perfomed synchronously 
addPlot jobQueue p batchId = unsafeThrow "Cannot batch non robust plot command"

setRunning :: JobQueue -> JobQueue
setRunning jobQueue = case Q.peek jobQueue.queue of
  Nothing -> jobQueue { running = Nothing }
  Just job -> jobQueue { running = pure job, queue = Q.tail jobQueue.queue }

runFirst :: Size -> Bounds -> JobQueue -> Aff (Maybe JobResult)
runFirst canvasSize bounds jobQueue = runMaybeJob (runJob canvasSize) jobQueue.cancelled maybeJob
  where
  maybeJob = Q.peek jobQueue.queue

isCancelled :: JobQueue -> Id -> Boolean
isCancelled jobQueue id = elem id jobQueue.cancelled

--------------------------------------------------------------------
-- Internal functions 
--------------------------------------------------------------------

batchSegmentCount :: Int
batchSegmentCount = 5

insertAll :: forall a f b. Foldable f => Ord b => (a -> b) -> f a -> S.Set b -> S.Set b
insertAll mapper toInsert set = foldr (S.insert <<< mapper) set toInsert

cancelRunning :: JobQueue -> JobQueue
cancelRunning jobQueue = case jobQueue.running of
  Nothing -> jobQueue
  Just job -> jobQueue { cancelled = S.insert job.id jobQueue.cancelled, running = Nothing }

isRunning :: (Job -> Boolean) -> JobQueue -> Boolean
isRunning check jobQueue = case jobQueue.running of
  Nothing -> false
  Just job -> check job

runMaybeJob :: (Job -> Aff (DrawCommand Unit)) -> S.Set Id -> Maybe Job -> Aff (Maybe JobResult)
runMaybeJob _ _ Nothing = pure Nothing

runMaybeJob runner cancelled (Just job) =
  if elem job.id cancelled then
    pure Nothing
  else do
    drawCommands <- runner job
    pure $ Just { job, drawCommands }

runJob :: Size -> Job -> Aff (DrawCommand Unit)
runJob canvasSize job = computePlotAsync canvasSize job.command

addJob :: Id -> JobQueue -> PlotCommand -> JobQueue
addJob batchId jobQueue command = jobQueue { queue = newQueue, currentId = newCurrentId }
  where
  newCurrentId = jobQueue.currentId + 1

  newQueue = Q.push jobQueue.queue { id: newCurrentId, command, batchId }

segmentRobust :: Bounds -> XYBounds -> Expression -> String -> Array PlotCommand
segmentRobust fullXBounds bounds expression label = commands
  where
  rangeX = bounds.xBounds.upper - bounds.xBounds.lower

  rangeY = bounds.yBounds.upper - bounds.yBounds.lower

  segmentWidth = rangeX / (toRational batchSegmentCount)

  batchBreakpoints = map (toRational >>> toDomainX) $ 0 .. batchSegmentCount

  commands = zipWith toPlotCommand batchBreakpoints (fromMaybe [] (tail batchBreakpoints))

  toDomainX :: Rational -> Rational
  toDomainX segmentX = (segmentX * segmentWidth) + bounds.xBounds.lower

  toPlotCommand :: Rational -> Rational -> PlotCommand
  toPlotCommand lower upper = RobustPlot commandBounds fullXBounds expression label
    where
    commandBounds = bounds { xBounds = { lower, upper } }

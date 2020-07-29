module Plot.JobBatcher
  ( JobQueue
  , Job
  , JobResult
  , cancelAll
  , addPlot
  , addParametricPlot
  , initialJobQueue
  , hasJobs
  , runFirst
  , setRunning
  , isCancelled
  , clearCancelled
  , countJobs
  ) where

import Prelude
import Data.Array (elem, foldl, foldr)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import Data.Set (Set, empty, insert) as S
import Data.Tuple (Tuple)
import Draw.Commands (DrawCommand)
import Effect (Effect)
import Effect.Aff (Canceler, Error, makeAff, nonCanceler)
import Effect.Console (log)
import Effect.Exception (try)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx)
import Misc.Array (split)
import Misc.ExpectAff (ExpectAff, MaybeExpectAff)
import Misc.Queue (Queue, empty, null, peek, push, tail, toList, length) as Q
import Plot.Commands (PlotCommand(..), Depth)
import Plot.FunctionSegments (segmentDomain)
import Plot.ParametricSegments (segmentParametricDomain)
import Plot.PlotController (computePlotAsync)
import Plot.RoughFunctionPlot (evaluateWithX)
import Plot.RoughParametricPlot (evaluateWithT)
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

-- | An empty `Job` queue. 
initialJobQueue :: JobQueue
initialJobQueue =
  { cancelled: S.empty
  , currentId: 0
  , queue: Q.empty
  , running: Nothing
  }

-- | Whether the `Job` queue has any pending `Job`s.
-- |
-- | Running time: `O(1)`
hasJobs :: JobQueue -> Boolean
hasJobs jobQueue = not $ Q.null jobQueue.queue

-- | Counts the number of pending `Job`s in queue not including the running job.
-- |
-- | Running time: `O(1)`
countJobs :: JobQueue -> Int
countJobs jobQueue = Q.length jobQueue.queue

-- | Empties the set of cancelled `Job`s. 
-- |
-- | Running time: `O(1)`
clearCancelled :: JobQueue -> JobQueue
clearCancelled = _ { cancelled = S.empty }

-- | Adds all the `Job`s that are currently pending to the set of cancelled `Job`s. If there is a `Job` running then that is canclled also.
-- |
-- | Running time: `O(n)`
cancelAll :: JobQueue -> JobQueue
cancelAll jobQueue = cancelRunning $ jobQueue { cancelled = cancelled, queue = Q.empty }
  where
  cancelled = insertAll (_.id) (Q.toList jobQueue.queue) jobQueue.cancelled

-- | Adds a plot with its associated `batchId` to the `Job` queue.
-- |
-- | Running time: `O(batchSegmentCount * (batchSegmentCount - 1))`
addPlot :: Number -> Int -> JobQueue -> XYBounds -> Expression -> Id -> ExpectAff JobQueue
addPlot accuracyTarget batchSegmentCount jobQueue bounds expression batchId = do
  segmentedPlotsOrError <- makeAff $ runSegmentFunctionRobust accuracyTarget batchSegmentCount bounds expression
  case segmentedPlotsOrError of
    Left error -> pure $ Left error
    Right segmentedPlots -> pure $ Right $ foldl (addJob batchId) jobQueue segmentedPlots

-- | Adds a paremetric plot with its associated `batchId` to the `Job` queue.
-- |
-- | Running time: `O(batchSegmentCount * (batchSegmentCount - 1))`
addParametricPlot :: Number -> Int -> JobQueue -> XYBounds -> Bounds -> Expression -> Expression -> Id -> ExpectAff JobQueue
addParametricPlot accuracyTarget batchSegmentCount jobQueue bounds domain xExpression yExpression batchId = do
  segmentedPlotsOrError <- makeAff $ runSegmentParametricRobust accuracyTarget batchSegmentCount bounds domain xExpression yExpression
  case segmentedPlotsOrError of
    Left error -> pure $ Left error
    Right segmentedPlots -> pure $ Right $ foldl (addJob batchId) jobQueue segmentedPlots

-- | Sets the `Job` at the front of the queue as running.
-- |
-- | Running time: `O(1)`
setRunning :: JobQueue -> JobQueue
setRunning jobQueue = case Q.peek jobQueue.queue of
  Nothing -> jobQueue { running = Nothing }
  Just job -> jobQueue { running = pure job, queue = Q.tail jobQueue.queue }

-- | Executes the `Job` at the front of the queue.
runFirst :: Size -> JobQueue -> MaybeExpectAff JobResult
runFirst canvasSize jobQueue = runMaybeJob (runJob canvasSize) jobQueue.cancelled maybeJob
  where
  maybeJob = Q.peek jobQueue.queue

-- | Whether the `Job` with the given `Id` has been cancelled or not.
isCancelled :: JobQueue -> Id -> Boolean
isCancelled jobQueue id = elem id jobQueue.cancelled

--------------------------------------------------------------------
-- Internal functions 
--------------------------------------------------------------------
insertAll :: forall a f b. Foldable f => Ord b => (a -> b) -> f a -> S.Set b -> S.Set b
insertAll mapper toInsert set = foldr (S.insert <<< mapper) set toInsert

runSegmentFunctionRobust :: Number -> Int -> XYBounds -> Expression -> (Either Error (Either Error (Array PlotCommand)) -> Effect Unit) -> Effect Canceler
runSegmentFunctionRobust accuracyTarget batchSegmentCount bounds expression callback = do
  result <-
    try
      $ do
          log "Segmenting..."
          pure $ segmentFunctionRobust accuracyTarget batchSegmentCount bounds expression
  callback $ Right $ result
  pure nonCanceler

runSegmentParametricRobust :: Number -> Int -> XYBounds -> Bounds -> Expression -> Expression -> (Either Error (Either Error (Array PlotCommand)) -> Effect Unit) -> Effect Canceler
runSegmentParametricRobust accuracyTarget batchSegmentCount bounds domain xExpression yExpression callback = do
  result <-
    try
      $ do
          log "Segmenting..."
          pure $ segmentParametricRobust accuracyTarget batchSegmentCount bounds domain xExpression yExpression
  callback $ Right $ result
  pure nonCanceler

cancelRunning :: JobQueue -> JobQueue
cancelRunning jobQueue = case jobQueue.running of
  Nothing -> jobQueue
  Just job -> jobQueue { cancelled = S.insert job.id jobQueue.cancelled, running = Nothing }

isRunning :: (Job -> Boolean) -> JobQueue -> Boolean
isRunning check jobQueue = case jobQueue.running of
  Nothing -> false
  Just job -> check job

runMaybeJob :: (Job -> ExpectAff (DrawCommand Unit)) -> S.Set Id -> Maybe Job -> MaybeExpectAff JobResult
runMaybeJob _ _ Nothing = pure Nothing

runMaybeJob runner cancelled (Just job) =
  if elem job.id cancelled then
    pure Nothing
  else do
    drawCommandsOrError <- runner job
    case drawCommandsOrError of
      Left error -> pure $ Just $ Left error
      Right drawCommands -> pure $ Just $ Right { job, drawCommands }

runJob :: Size -> Job -> ExpectAff (DrawCommand Unit)
runJob canvasSize job = computePlotAsync canvasSize job.command

addJob :: Id -> JobQueue -> PlotCommand -> JobQueue
addJob batchId jobQueue command = jobQueue { queue = newQueue, currentId = newCurrentId }
  where
  newCurrentId = jobQueue.currentId + 1

  newQueue = Q.push jobQueue.queue { id: newCurrentId, command, batchId }

segmentFunctionRobust :: Number -> Int -> XYBounds -> Expression -> Array PlotCommand
segmentFunctionRobust accuracyTarget batchSegmentCount bounds expression = commands
  where
  evaluator = evaluateWithX expression

  domainSegments = segmentDomain { accuracyTarget, evaluator, l: bounds.xBounds.lower, u: bounds.xBounds.upper }

  splitDomainSegments = split batchSegmentCount domainSegments

  commands = map toPlotCommand splitDomainSegments

  toPlotCommand :: Array (Tuple Depth Approx) -> PlotCommand
  toPlotCommand segments = RobustFunctionPlot bounds expression segments accuracyTarget

segmentParametricRobust :: Number -> Int -> XYBounds -> Bounds -> Expression -> Expression -> Array PlotCommand
segmentParametricRobust accuracyTarget batchSegmentCount bounds domain xExpression yExpression = commands
  where
  evaluator = evaluateWithT xExpression yExpression

  domainSegments = segmentParametricDomain { accuracyTarget, evaluator, l: domain.lower, u: domain.upper }

  splitDomainSegments = split batchSegmentCount domainSegments

  commands = map toPlotCommand splitDomainSegments

  toPlotCommand :: Array (Tuple Depth Approx) -> PlotCommand
  toPlotCommand segments = RobustParametricPlot bounds domain xExpression yExpression segments accuracyTarget

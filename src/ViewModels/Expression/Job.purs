module ViewModels.Expression.Job where

import Prelude
import Data.Array (cons, uncons)
import Data.Foldable (any, sum)
import Data.Maybe (Maybe(..))
import Misc.ExpectAff (MaybeExpectAff)
import Plot.JobBatcher (Job, JobResult, cancelAll, clearCancelled, countJobs, hasJobs, isCancelled, runFirst, setRunning)
import Types (Size)
import ViewModels.Expression (ExpressionViewModel(..))

countBatches :: Array ExpressionViewModel -> Int
countBatches = sum <<< map toBatchCount
  where
  toBatchCount :: ExpressionViewModel -> Int
  toBatchCount (Function vm) = countJobs vm.queue

  toBatchCount (Parametric vm) = countJobs vm.queue

queueHasJobs :: ExpressionViewModel -> Boolean
queueHasJobs (Function vm) = hasJobs vm.queue

queueHasJobs (Parametric vm) = hasJobs vm.queue

anyHasJobs :: Array ExpressionViewModel -> Boolean
anyHasJobs = any queueHasJobs

cancelAllJobs :: Array ExpressionViewModel -> Array ExpressionViewModel
cancelAllJobs = map cancel
  where
  cancel :: ExpressionViewModel -> ExpressionViewModel
  cancel (Function vm) = Function $ vm { queue = cancelAll vm.queue }

  cancel (Parametric vm) = Parametric $ vm { queue = cancelAll vm.queue }

clearCancelledJobs :: Array ExpressionViewModel -> Array ExpressionViewModel
clearCancelledJobs = map clear
  where
  clear :: ExpressionViewModel -> ExpressionViewModel
  clear (Function vm) = Function $ vm { queue = clearCancelled vm.queue }

  clear (Parametric vm) = Parametric $ vm { queue = clearCancelled vm.queue }

isJobCancelled :: Job -> Array ExpressionViewModel -> Boolean
isJobCancelled job = any check
  where
  check :: ExpressionViewModel -> Boolean
  check (Function vm) = isCancelled vm.queue job.id

  check (Parametric vm) = isCancelled vm.queue job.id

setFirstRunningJob :: Array ExpressionViewModel -> Array ExpressionViewModel
setFirstRunningJob vms = case uncons vms of
  Nothing -> vms
  Just { head, tail } ->
    if queueHasJobs head then
      cons (go head) tail
    else
      cons head $ setFirstRunningJob tail
  where
  go :: ExpressionViewModel -> ExpressionViewModel
  go (Function vm) = Function $ vm { queue = setRunning vm.queue }

  go (Parametric vm) = Parametric $ vm { queue = setRunning vm.queue }

runFirstJob :: Size -> Array ExpressionViewModel -> MaybeExpectAff JobResult
runFirstJob size vms = case uncons vms of
  Nothing -> pure Nothing
  Just { head, tail } ->
    if queueHasJobs head then
      go head
    else
      runFirstJob size tail
  where
  go :: ExpressionViewModel -> MaybeExpectAff JobResult
  go (Function vm) = runFirst size vm.queue

  go (Parametric vm) = runFirst size vm.queue

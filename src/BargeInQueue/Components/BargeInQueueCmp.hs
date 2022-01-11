{-# LANGUAGE NoImplicitPrelude #-}

module BargeInQueue.Components.BargeInQueueCmp
    ( BargeInQueueCmp(..)
    ) where

import           Verset

import qualified BargeInQueue.Core as C
import qualified BargeInQueue.Components.UserCmp as CUsr
import qualified BargeInQueue.Components.LogCmp as CL

data BargeInQueueCmp m = BargeInQueueCmp
  { bqVersion :: !Text
  , bqStartQueue :: !(CUsr.UserCmp m -> m ())
  , bqAddPendingWorkItem :: !(C.NewWorkItem -> m (Either Text C.WorkItemId))
  , bqAddActiveQueueItem :: !(C.NewWorkItem -> m (Either Text C.QueueItemId))
  , bqAbortWorkItem :: !(C.WorkItemId -> m ())
  , bqSetWorkItemDone :: !(C.WorkItemId -> m ())
  , bqExpireQueueItem :: !(C.QueueItemId -> m ())
  , bqExtendTimeout :: !(C.QueueItemId -> UTCTime -> m ())
  , bqFailQueueItem :: !(C.QueueItemId -> m ())
  , bqListUnqueuedUnblockedWorkItems :: !(Int -> m (Either Text [C.WorkItem]))
  , bqTriggerPoll :: !(m ())
  , bqQueueAllUnblockedWorkItems :: !(m (Either Text ()))
  , bqGotHeartBeat :: !(C.QueueItemId -> m (Either Text ()))

  , bqListSystems :: !(m (Either Text [C.SystemConfig]))
  , bqGetSystem :: !(C.SystemId -> m (Either Text (Maybe C.SystemConfig)))
  , bqGetQueueItem :: !(C.QueueItemId -> m (Either Text C.QueueItem))
  , bqGetWorkItem :: !(C.WorkItemId -> m (Either Text C.WorkItem))
  , bqGetWorkType :: !(C.WorkTypeId -> m (Either Text C.WorkType))

  , bqLog :: !(m (CL.LogCmp m))
  }

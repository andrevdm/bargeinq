{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module BargeInQueue.Components.RepoCmp
    ( RepoCmp(..)
    ) where

import           Verset

import qualified BargeInQueue.Core as C

data RepoCmp m = RepoCmp
  { rpListSystems :: !(m (Either Text [C.SystemConfig]))
  , rpGetSystem :: !(C.SystemId -> m (Either Text (Maybe C.SystemConfig)))
  , rpFetchNextActiveItem :: !(C.SystemConfig -> m (Either Text (Maybe C.DequeuedActiveItem)))
  , rpDeleteWorkItem :: !(C.WorkItemId -> m (Either Text ()))
  , rpDeleteQueueItem :: !(C.QueueItemId -> m (Either Text ()))
  , rpExpireQueueItem :: !(C.QueueItemId -> m (Either Text ()))
  , rpFailQueueItem :: !(C.QueueItemId -> m (Either Text ()))
  , rpPauseWorkItem :: !(C.WorkItemId -> NominalDiffTime -> m (Either Text ()))
  , rpAbortWorkItem :: !(C.WorkItemId -> m (Either Text ()))
  , rpGetQueueItem :: !(C.QueueItemId -> m (Either Text C.QueueItem))
  , rpGetWorkItem :: !(C.WorkItemId -> m (Either Text C.WorkItem))
  , rpGetWorkType :: !(C.WorkTypeId -> m (Either Text C.WorkType))
  , rpUpdateWorkItemForRetry :: !(C.WorkItem -> m (Either Text ()))
  , rpCreateQueueItem :: !(C.WorkItemId -> UTCTime -> m (Either Text C.QueueItemId))
  , rpListUnqueuedUnblockedWorkItems :: !(C.SystemId -> Int -> m (Either Text [C.WorkItem]))
  , rpQueueAllUnblockedWorkItems :: !(C.SystemId -> m (Either Text ()))
  , rpExtendTimeout :: !(C.QueueItemId -> UTCTime -> m (Either Text ()))
  , rpGetMissedHeartbeats :: !(C.SystemId -> m (Either Text [C.QueueItem]))
  , rpGetFailHeartbeats :: !(C.SystemId -> m (Either Text [C.QueueItem]))
  , rpFailAllHeartbeatExpired :: !(C.SystemId -> m (Either Text ()))
  , rpGotHeartbeat :: !(C.QueueItemId -> m (Either Text ()))
  , rpAddPendingWorkItem :: !(C.NewWorkItem -> m (Either Text C.WorkItemId))
  , rpAddActiveQueueItem :: !(C.NewWorkItem -> m (Either Text C.QueueItemId))
  }


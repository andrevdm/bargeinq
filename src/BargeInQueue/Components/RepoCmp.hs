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
  , rpDeletePendingWorkItem :: !(C.PendingWorkItemId -> m (Either Text ()))
  , rpDeleteWorkItem :: !(C.WorkItemId -> m (Either Text ()))
  , rpExpireQueueItem :: !(C.QueueItemId -> m (Either Text ()))
  , rpPauseWorkItem :: !(C.WorkItemId -> NominalDiffTime -> m (Either Text ()))
  , rpGetWorkItem :: !(C.WorkItemId -> m (Either Text C.WorkItem))
  , rpGetWorkType :: !(C.WorkTypeId -> m (Either Text C.WorkType))
  , rpUpdateWorkItemForRetry :: !(C.WorkItem -> m (Either Text ()))
  , rpCreatePendingWorkItem :: !(C.WorkItemId -> m (Either Text C.PendingWorkItemId))
  , rpCreateQueueItem :: !(C.PendingWorkItemId -> UTCTime -> m (Either Text C.QueueItemId))
  }


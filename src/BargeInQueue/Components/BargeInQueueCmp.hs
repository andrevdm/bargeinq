{-# LANGUAGE NoImplicitPrelude #-}

module BargeInQueue.Components.BargeInQueueCmp
    ( BargeInQueueCmp(..)
    ) where

import           Verset

import qualified BargeInQueue.Core as C
import qualified BargeInQueue.Components.UserCmp as CUsr

data BargeInQueueCmp m = BargeInQueueCmp
  { bqVersion :: !Text
  , bqStartQueue :: !(CUsr.UserCmp m -> m ())
  , bqQueueWork :: !(C.PendingWorkItems -> C.QueueWorkItems -> m ())
  , bqSetWorkItemDone :: !(C.WorkItemId -> m ())
  , bqExpireQueueItem :: !(C.QueueItemId -> m ())
  , bqFailQueueItem :: !(C.QueueItemId -> m ())
  , bqListUnqueuedUnblockedWorkItems :: !(C.SystemId -> Int -> m (Either Text [C.WorkItem]))
  , bqQueueAllUnblockedWorkItems :: !(C.SystemId -> m (Either Text ()))
  }

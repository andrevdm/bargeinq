{-# LANGUAGE NoImplicitPrelude #-}

module BargeInQueue.Components.UserCmp
    ( UserCmp(..)
    , ProcessItemResult(..)
    ) where

import           Verset

import qualified BargeInQueue.Core as C


data ProcessItemResult
  = PirSuccess
  | PirError Text Text
  deriving (Show, Eq)


data UserCmp m = UserCmp
  { usrQueueStarting :: m ()
  , usrProcessActiveItem :: !(C.DequeuedActiveItem -> m ())
  , usrNotifyWorkItemTimeout :: !(C.DequeuedActiveItem -> m ())
  , usrNotifyWorkItemFailed :: !(C.DequeuedActiveItem -> C.FailReason -> m ())
  , usrNotifyRetrypingWorkItem :: !(C.QueueItemId -> C.WorkItem -> m ())
  , usrNotifyWorkItemFailedNoMoreRetries :: !(C.WorkItem -> m ())
  , usrNotifyWorkItemSucceeded :: !(C.WorkItem -> m ())
  , usrNotifyHeartbeatsMissed :: !(Maybe ([C.QueueItem] -> m ()))
  , usrNotifyHeartbeatsFailed :: !([C.QueueItemId] -> m ())
  }

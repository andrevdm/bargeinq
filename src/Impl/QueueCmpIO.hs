{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Impl.QueueCmpIO
    ( newQueueCmpIO
    ) where

import           Protolude
import           Data.Time (UTCTime)
import           Data.UUID (UUID)

import qualified Components.QueueCmp as CQ


newQueueCmpIO
  :: forall m.
     (MonadIO m)
  => CQ.QueueCmp m
newQueueCmpIO =
  CQ.QueueCmp
    { CQ.qQueueWork = queueWork
    , CQ.qStartQueue = startQueue
    }


queueWork
  :: forall m.
     (MonadIO m)
  => CQ.PendingWorkItems
  -> CQ.QueueWorkItems
  -> m ()
queueWork (CQ.PendingWorkItems pis) (CQ.QueuedWorkItems qis) = do
  pass


startQueue
  :: forall m.
     (MonadIO m)
  => CQ.SystemId
  -> m CQ.QueueHandle
startQueue _sid = do
  undefined

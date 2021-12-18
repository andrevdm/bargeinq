{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Impl.QueueCmpIO
    ( newQueueCmpIO
    ) where

import           Protolude hiding (threadDelay)
import           Data.Time (UTCTime)
import           Data.UUID (UUID)
import           UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Async as UA
import qualified UnliftIO.Concurrent as UC

import qualified Components.QueueCmp as CQ


newQueueCmpIO
  :: forall m.
     (MonadUnliftIO m)
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
queueWork (CQ.PendingWorkItems pws) (CQ.QueueWorkItems qws) = do
  pass


startQueue
  :: forall m.
     (MonadUnliftIO m)
  => CQ.SystemId
  -> m (UA.Async ())
startQueue _sid = UA.async . forever $ do
  UC.threadDelay 1000
  pass

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Impl.QueueCmpIO
    ( newQueueCmpIO
    ) where

import           Verset hiding (threadDelay)
import           UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Async as UA
import qualified UnliftIO.Concurrent as UC

import qualified Components.LogCmp as CL
import qualified Components.PsqlCmp as CPg
import qualified Components.QueueCmp as CQ


newQueueCmpIO
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> CL.LogCmp m
  -> CQ.QueueCmp m
newQueueCmpIO pgCmp lgCmp =
  CQ.QueueCmp
    { CQ.qQueueWork = queueWork
    , CQ.qStartQueue = startQueue pgCmp lgCmp
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
  => CPg.PsqlCmp m
  -> CL.LogCmp m
  -> CQ.SystemId
  -> m ()
startQueue pgCmp lgCmp _sid = do
  CPg.pgListenForNotifications pgCmp "test" (CL.logDebug' lgCmp "LISTEN> ")

  void . UA.async . forever $ do
    UC.threadDelay 1000

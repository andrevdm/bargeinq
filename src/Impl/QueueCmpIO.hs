{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Impl.QueueCmpIO
    ( newQueueCmpIO
    ) where

import           Verset hiding (threadDelay)
import qualified Data.Text as Txt
import qualified Data.UUID as UU
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
  -> CQ.SystemId
  -> CQ.QueueCmp m
newQueueCmpIO pgCmp lgCmp (CQ.SystemId sid) = do
  let chan = CPg.ChanName $ "c" <> Txt.replace "-" "" (UU.toText sid)
  CQ.QueueCmp
    { CQ.qQueueWork = queueWork
    , CQ.qStartQueue = startQueue pgCmp lgCmp chan
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
  -> CPg.ChanName
  -> m ()
startQueue pgCmp lgCmp chanName = do
  CPg.pgListenForNotifications pgCmp chanName (CL.logDebug' lgCmp "LISTEN> ")
  CPg.pgNotify pgCmp chanName "text"

  void . UA.async . forever $ do
    UC.threadDelay 1000

  CPg.pgNotify pgCmp chanName "done"
  CPg.pgNotify pgCmp chanName "done!!!"

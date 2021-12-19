{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module BargeInQueue.Impl.QueueCmpIO
    ( newQueueCmpIO
    ) where

import           Verset hiding (threadDelay)
import qualified Data.Text as Txt
import qualified Data.UUID as UU
import           UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Async as UA
import qualified UnliftIO.Concurrent as UC

import qualified BargeInQueue.Core as C
import qualified BargeInQueue.Components.LogCmp as CL
import qualified BargeInQueue.Components.PsqlCmp as CPg
import qualified BargeInQueue.Components.QueueCmp as CQ
import qualified BargeInQueue.Threading as Th


newQueueCmpIO
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> CL.LogCmp m
  -> C.SystemId
  -> CQ.QueueCmp m
newQueueCmpIO pgCmp lgCmp (C.SystemId sid) = do
  let chan = CPg.ChanName $ "c" <> Txt.replace "-" "" (UU.toText sid)
  CQ.QueueCmp
    { CQ.qQueueWork = queueWork
    , CQ.qStartQueue = startQueue pgCmp lgCmp chan
    }


queueWork
  :: forall m.
     (MonadIO m)
  => C.PendingWorkItems
  -> C.QueueWorkItems
  -> m ()
queueWork (C.PendingWorkItems pws) (C.QueueWorkItems qws) = do
  pass


startQueue
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> CL.LogCmp m
  -> CPg.ChanName
  -> m ()
startQueue pgCmp lgCmp chanName = do
  pollGate <- Th.newOpenGate

  CPg.pgListenForNotifications pgCmp chanName $ \n -> do
    CL.logDebug' lgCmp "LISTEN> " n
    Th.openGate pollGate

  void . UA.async $ runPollLoop pollGate tryGetActiveItem
  void . UA.async $ runTriggerPoll 10 pollGate --TODO get poll period


-- | See if there is actually an item to work with
tryGetActiveItem
  :: forall m.
     (MonadUnliftIO m)
  => m Bool
tryGetActiveItem = do
  putText "tryGetActive"
  pure False


-- | Main poll loop. Calls `fn` when it is time to poll
runPollLoop
  :: forall m.
     (MonadUnliftIO m)
  => Th.Gate
  -> m Bool
  -> m ()
runPollLoop gate fn = forever $ do
  Th.waitForOpenGateAndClose gate
  runFetch

  where
    runFetch = do
      putText "polling"
      gotItem <- fn
      -- If there is an item then there is probably another, try again immediately
      when gotItem runFetch


-- | Triggers the poll periodically
runTriggerPoll
  :: forall m.
     (MonadUnliftIO m)
  => Int
  -> Th.Gate
  -> m ()
runTriggerPoll seconds gate = forever $ do
  UC.threadDelay $ 1000000 * seconds
  Th.openGate gate

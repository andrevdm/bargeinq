{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module BargeInQueue.Impl.QueueCmpIO
    ( newQueueCmpIO
    ) where

import           Verset hiding (threadDelay)
import qualified Data.Text as Txt
import qualified Data.UUID as UU
import           Control.Lens ((^.))
import           UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Async as UA
import qualified UnliftIO.Concurrent as UC
import           Text.Pretty.Simple (pPrint)

import qualified BargeInQueue.Core as C
import qualified BargeInQueue.Components.LogCmp as CL
import qualified BargeInQueue.Components.PsqlCmp as CPg
import qualified BargeInQueue.Components.RepoCmp as CR
import qualified BargeInQueue.Components.QueueCmp as CQ
import qualified BargeInQueue.Components.UserCmp as CUsr
import qualified BargeInQueue.Threading as Th


newQueueCmpIO
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> CL.LogCmp m
  -> CR.RepoCmp m
  -> CUsr.UserCmp m
  -> C.SystemConfig
  -> CQ.QueueCmp m
newQueueCmpIO pgCmp lgCmp repoCmp usrCmp sys = do
  let (C.SystemId sysId) = sys ^. C.sysId
  let chan = CPg.ChanName $ "c" <> Txt.replace "-" "" (UU.toText sysId)
  CQ.QueueCmp
    { CQ.qQueueWork = queueWork
    , CQ.qStartQueue = startQueue sys pgCmp lgCmp repoCmp usrCmp chan
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
  => C.SystemConfig
  -> CPg.PsqlCmp m
  -> CL.LogCmp m
  -> CR.RepoCmp m
  -> CUsr.UserCmp m
  -> CPg.ChanName
  -> m ()
startQueue sys pgCmp lgCmp repoCmp usrCmp chanName = do
  pollGate <- Th.newOpenGate

  CPg.pgListenForNotifications pgCmp chanName $ \n -> do
    CL.logDebug' lgCmp "LISTEN> " n
    Th.openGate pollGate

  void . UA.async $ runPollLoop pollGate (tryGetActiveItem repoCmp sys)

  CL.logDebug lgCmp $ "Starting poll: " <> show (sys ^. C.sysPollPeriodSeconds) <> " seconds"
  void . UA.async $ runTriggerPoll (sys ^. C.sysPollPeriodSeconds) pollGate


-- | See if there is actually an item to work with
tryGetActiveItem
  :: forall m.
     (MonadUnliftIO m)
  => CR.RepoCmp m
  -> C.SystemConfig
  -> m Bool
tryGetActiveItem repoCmp sys = do
  putText "tryGetActive"
  CR.rpFetchNextActiveItem repoCmp sys >>= \case
    Left e -> do
      print e --TODO log + error
      pure True

    Right Nothing ->
      pure False -- Nothing was returned

    Right (Just qi) -> do
      if isJust (qi ^. CR.dqaDequeuedAt)
        then do
          --TODO CUsr.usrProcessQueuedItem (qi ^. CR.dqaQueueId) (qi ^. CR.dqqWorkItemId) (qi ^. CR.dqaWorkTypeId) (qi ^. CR.dqaWorkItemName)
          pPrint qi
          pure True
        else do
          --TODO lock timeout expired, log, remove from queue, check retries etc
          pure True


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

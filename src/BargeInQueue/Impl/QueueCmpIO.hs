{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BargeInQueue.Impl.QueueCmpIO
    ( newQueueCmpIO
    , getWorkType
    ) where

import           Verset hiding (threadDelay)
import qualified Data.Text as Txt
import qualified Data.Time as DT
import qualified Data.UUID as UU
import           Control.Lens ((^.), (?~), (%~))
import           UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Async as UA
import qualified UnliftIO.Concurrent as UC
import qualified UnliftIO.Exception as UE

import qualified BargeInQueue.Core as C
import qualified BargeInQueue.Components.DateCmp as CDt
import qualified BargeInQueue.Components.LogCmp as CL
import qualified BargeInQueue.Components.PsqlCmp as CPg
import qualified BargeInQueue.Components.RepoCmp as CR
import qualified BargeInQueue.Components.QueueCmp as CQ
import qualified BargeInQueue.Components.UserCmp as CUsr
import qualified BargeInQueue.Components.EnvCmp as CEnv
import qualified BargeInQueue.Threading as Th


newQueueCmpIO
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> CL.LogCmp m
  -> CR.RepoCmp m
  -> CEnv.EnvCmp m
  -> CDt.DateCmp m
  -> C.SystemConfig
  -> Maybe (Text, Int)
  -> m (CQ.QueueCmp m)
newQueueCmpIO pgCmp lgCmp repoCmp envCmp dtCmp sys hostMax = do
  pollGate <- Th.newOpenGate

  let (C.SystemId sysId) = sys ^. C.sysId
  let chan = CPg.ChanName $ "c" <> Txt.replace "-" "" (UU.toText sysId)
  pure CQ.QueueCmp
    { CQ.qStartQueue = startQueue sys pgCmp lgCmp repoCmp envCmp dtCmp chan pollGate hostMax
    , CQ.qCheckUnblocked = checkUnblocked repoCmp sys
    , CQ.qTriggerPoll = Th.openGate pollGate
    }


startQueue
  :: forall m.
     (MonadUnliftIO m)
  => C.SystemConfig
  -> CPg.PsqlCmp m
  -> CL.LogCmp m
  -> CR.RepoCmp m
  -> CEnv.EnvCmp m
  -> CDt.DateCmp m
  -> CPg.ChanName
  -> Th.Gate
  -> Maybe (Text, Int)
  -> m ()
startQueue sys pgCmp logCmp repoCmp envCmp dtCmp chanName pollGate hostMax = do
  CPg.pgListenForNotifications pgCmp chanName $ \n -> do
    CL.logTest' logCmp "LISTEN> " n
    Th.openGate pollGate

  void . UA.async $ runPollLoop repoCmp sys pollGate (tryProcessNextActiveItem repoCmp envCmp logCmp dtCmp sys hostMax)

  CL.logDebug logCmp $ "Starting poll: " <> show (sys ^. C.sysPollPeriodSeconds) <> " seconds"
  void . UA.async $ runTriggerPoll (sys ^. C.sysPollPeriodSeconds) pollGate

  case sys ^. C.sysHeartbeatCheckPeriodSeconds of
    Nothing -> pass
    (Just hbSeconds) -> void . UA.async $ runHeartbeatChecks pgCmp logCmp repoCmp envCmp hbSeconds sys


-- | See if there is actually an item to work with
tryProcessNextActiveItem
  :: forall m.
     (MonadUnliftIO m)
  => CR.RepoCmp m
  -> CEnv.EnvCmp m
  -> CL.LogCmp m
  -> CDt.DateCmp m
  -> C.SystemConfig
  -> Maybe (Text, Int)
  -> m Bool
tryProcessNextActiveItem repoCmp envCmp logCmp dtCmp sys hostMax = do
  usrCmp <- CEnv.envDemandUser envCmp

  CR.rpFetchNextActiveItem repoCmp sys hostMax >>= \case
    Right Nothing -> pure False -- Nothing was returned
    Left e -> errorDequeueing e >> pure False -- Return false in case there is a DB error. Returning True could end in an error loop

    Right (Just dqi) -> do
      case (dqi ^. C.dqaFailReason, dqi ^. C.dqaDequeuedAt) of
          -- Process active item
        (Nothing, Nothing) -> gotNewActiveItem usrCmp dqi >> pure True
          -- An explicit fail was set
        (Just fr, _) -> queuedItemFailed usrCmp dqi fr >> pure True
          -- If the item already had dequeued_at set then it was previously dequeued
          -- The only way it could be returned here is if the lock period expired
          -- i.e. if it timed out
        (_, Just _) -> lastLockTimeoutExpiredForActiveItem usrCmp dqi >> pure True

  where
    errorDequeueing e = do
      CL.logError' logCmp "Exception dequeuing" e

    gotNewActiveItem usrCmp dqi =
      catchErrorAsync "user handler" $
        CUsr.usrProcessActiveItem usrCmp dqi

    queuedItemFailed usrCmp dqi fr = do
      catchErrorAsync "Notify work item failed" (CUsr.usrNotifyWorkItemFailed usrCmp dqi fr)
      -- pause the work item so that no other thread/process fetches it while this code is running
      void $ CR.rpPauseWorkItem repoCmp (dqi ^. C.dqaWorkItemId) 10
      -- delete the pending work item, as the pending action is done (failed)
      void $ CR.rpDeleteQueueItem repoCmp (dqi ^. C.dqaQueueId)
      -- retry. Run async so that the queue can get the next item so long. The pause call above should prevent race conditions
      catchErrorAsync "retry" $ retryWorkItem repoCmp usrCmp logCmp dtCmp envCmp sys dqi

    lastLockTimeoutExpiredForActiveItem usrCmp dqi = do
      catchErrorAsync "Notify work item timeout" (CUsr.usrNotifyWorkItemTimeout usrCmp dqi)
      -- pause the work item so that no other thread/process fetches it while this code is running
      void $ CR.rpPauseWorkItem repoCmp (dqi ^. C.dqaWorkItemId) 10
      -- delete the pending work item, as the pending action is done (failed)
      void $ CR.rpDeleteQueueItem repoCmp (dqi ^. C.dqaQueueId)
      -- retry. Run async so that the queue can get the next item so long. The pause call above should prevent race conditions
      catchErrorAsync "retry" $ retryWorkItem repoCmp usrCmp logCmp dtCmp envCmp sys dqi

    catchError n f =
      UE.catch f (\(e :: SomeException) -> CL.logError' logCmp ("Exception running " <> n) e)

    catchErrorAsync n f = void . UA.async $ catchError n (void f)



-- | Main poll loop. Calls `fn` when it is time to poll
retryWorkItem
  :: forall m.
     (MonadUnliftIO m)
  => CR.RepoCmp m
  -> CUsr.UserCmp m
  -> CL.LogCmp m
  -> CDt.DateCmp m
  -> CEnv.EnvCmp m
  -> C.SystemConfig
  -> C.DequeuedActiveItem
  -> m ()
retryWorkItem repoCmp usrCmp _logCmp dtCmp _envCmp sys dqi = do
  wi <- CR.rpGetWorkItem repoCmp (dqi ^. C.dqaWorkItemId) >>= \case
    Left e -> UE.throwString . Txt.unpack $ "Error running retry for " <> show (dqi ^. C.dqaWorkItemId) <> "\n" <> e
    Right r -> pure r

  if wi ^. C.wiRetriesLeft > 0
    then addRetry wi
    else noMoreRetries wi

  where
    addRetry wi = do
      now <- CDt.dtGetDate dtCmp
      let wi2 = wi & C.wiRetriesLeft %~ (`subtract` 1)
                   & C.wiAttempts %~ (+ 1)
                   & C.wiIgnoreUntil ?~ DT.addUTCTime 10 now

      -- Update the work item
      CR.rpUpdateWorkItemForRetry repoCmp wi2 >>= \case
        Right _ -> pass
        Left e -> UE.throwString . Txt.unpack $ "Error updating work item for retry" <> show (dqi ^. C.dqaWorkItemId) <> "\n" <> e

      -- Queue to make it active but locked for the backoff period
      let backoffUntil = DT.addUTCTime (getBackoff (wi ^. C.wiBackoffSeconds) (wi ^. C.wiAttempts) 120) now

      qid <- CR.rpCreateQueueItem repoCmp (wi ^. C.wiId) backoffUntil >>= \case
        Right q -> pure q
        Left e -> UE.throwString . Txt.unpack $ "Error creating queue item for retry" <> show (wi ^. C.wiId) <> "\n" <> e

      CUsr.usrNotifyRetryingWorkItem usrCmp qid wi


    noMoreRetries wi = do
      CR.rpDeleteWorkItem repoCmp (wi ^. C.wiId) >>= \case
        Right _ -> pass
        Left e -> UE.throwString . Txt.unpack $ "Error delete working item, no more retries: " <> show (wi ^. C.wiId) <> "\n" <> e

      checkUnblocked repoCmp sys
      CUsr.usrNotifyWorkItemFailedNoMoreRetries usrCmp wi



getWorkType
  :: (MonadIO m)
  => CEnv.EnvCmp m
  -> CR.RepoCmp m
  -> C.WorkTypeId
  -> m C.WorkType
getWorkType envCmp repoCmp wtid =
  CEnv.envGetCachedWorkType envCmp wtid >>= \case
    Just w -> pure w
    Nothing -> do
      CR.rpGetWorkType repoCmp wtid >>= \case
        Right w -> do
          CEnv.envCacheWorkType envCmp w
          pure w
        Left e ->
          UE.throwString . Txt.unpack $ "Error fetching work type for retry\n" <> e


getBackoff :: [Int] -> Int -> Int -> NominalDiffTime
getBackoff bs bAt bDefault' =
  let bDefault = fromMaybe bDefault' $ maximumMay bs in
  fromIntegral $ fromMaybe bDefault (atMay bs bAt)



-- | Main poll loop. Calls `fn` when it is time to poll
runPollLoop
  :: forall m.
     (MonadUnliftIO m)
  => CR.RepoCmp m
  -> C.SystemConfig
  -> Th.Gate
  -> m Bool
  -> m ()
runPollLoop repoCmp sys gate fn = forever $ do
  Th.waitForOpenGateAndClose gate
  runFetch

  where
    runFetch = do
      --CL.logDebug logCmp "polling"
      gotItem <- fn
      checkUnblocked repoCmp sys
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


checkUnblocked
  :: forall m.
     (MonadUnliftIO m)
  => CR.RepoCmp m
  -> C.SystemConfig
  -> m ()
checkUnblocked repoCmp sys = do
  when (sys ^. C.sysAutoQueueUnblocked) $ do
    _ <- CR.rpQueueAllUnblockedWorkItems repoCmp (sys ^. C.sysId)
    pass


runHeartbeatChecks
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> CL.LogCmp m
  -> CR.RepoCmp m
  -> CEnv.EnvCmp m
  -> Int
  -> C.SystemConfig
  -> m ()
runHeartbeatChecks _pgCmp logCmp repoCmp envCmp periodSeconds sys = forever $ do
  usrCmp <- CEnv.envDemandUser envCmp
  let sysId = sys ^. C.sysId

  UC.threadDelay $ 1000000 * periodSeconds

  -- Fail all that missed too many heartbeats
  -- Call this before the code below so that the failed ones are removed first
  qids <- CR.rpFailAllHeartbeatExpired repoCmp sysId >>= \case
    Right r -> pure r
    Left e -> CL.logError' logCmp "Error failing missed heartbeats" e >> pure []


  unless (null qids) $ CUsr.usrNotifyHeartbeatsFailed usrCmp qids

  -- Notify the user, if the want notifications of missing heartbeats
  case CUsr.usrNotifyHeartbeatsMissed usrCmp of
    Nothing -> pass
    Just notify -> do
      missed <- CR.rpGetMissedHeartbeats repoCmp sysId >>= \case
        Right r -> pure r
        Left e -> do
          CL.logError' logCmp "Error getting missed heartbeats" e
          pure []
      unless (null missed) $ notify missed



{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BargeInQueue.Impl.QueueCmpIO
    ( newQueueCmpIO
    ) where

import           Verset hiding (threadDelay)
import qualified Data.Text as Txt
import qualified Data.Time as DT
import qualified Data.UUID as UU
import qualified Data.UUID.V4 as UU
import           Control.Lens ((^.), (.~), (?~), (%~))
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
import qualified BargeInQueue.Threading as Th
import qualified BargeInQueue.Components.EnvCmp as CEnv


newQueueCmpIO
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> CL.LogCmp m
  -> CR.RepoCmp m
  -> CEnv.EnvCmp m
  -> CDt.DateCmp m
  -> C.SystemConfig
  -> CQ.QueueCmp m
newQueueCmpIO pgCmp lgCmp repoCmp envCmp dtCmp sys = do
  let (C.SystemId sysId) = sys ^. C.sysId
  let chan = CPg.ChanName $ "c" <> Txt.replace "-" "" (UU.toText sysId)
  CQ.QueueCmp
    { CQ.qQueueWork = queueWork
    , CQ.qStartQueue = startQueue sys pgCmp lgCmp repoCmp envCmp dtCmp chan
    }


queueWork
  :: forall m.
     (MonadIO m)
  => C.PendingWorkItems
  -> C.QueueWorkItems
  -> m ()
queueWork (C.PendingWorkItems _pws) (C.QueueWorkItems _qws) = do
  pass


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
  -> m ()
startQueue sys pgCmp logCmp repoCmp envCmp dtCmp chanName = do
  pollGate <- Th.newOpenGate

  CPg.pgListenForNotifications pgCmp chanName $ \n -> do
    CL.logTest' logCmp "LISTEN> " n
    Th.openGate pollGate

  void . UA.async $ runPollLoop pollGate (tryProcessNextActiveItem repoCmp envCmp logCmp dtCmp sys)

  CL.logDebug logCmp $ "Starting poll: " <> show (sys ^. C.sysPollPeriodSeconds) <> " seconds"
  void . UA.async $ runTriggerPoll (sys ^. C.sysPollPeriodSeconds) pollGate


-- | See if there is actually an item to work with
tryProcessNextActiveItem
  :: forall m.
     (MonadUnliftIO m)
  => CR.RepoCmp m
  -> CEnv.EnvCmp m
  -> CL.LogCmp m
  -> CDt.DateCmp m
  -> C.SystemConfig
  -> m Bool
tryProcessNextActiveItem repoCmp envCmp logCmp dtCmp sys = do
  putText "tryGetActive"
  usrCmp <- CEnv.envDemandUser envCmp

  CR.rpFetchNextActiveItem repoCmp sys >>= \case
    Right Nothing -> pure False -- Nothing was returned
    Left e -> errorDequeueing e >> pure False -- Return false in case there is a DB error. Returning True could end in an error loop

    -- If the item already had dqaDequeuedAt set then it was previously dequeued
    -- The only way it could be returned here is if the lock period expired
    -- i.e. if it timed out
    Right (Just dqi) -> do
      if isNothing (dqi ^. C.dqaDequeuedAt)
        then gotNewActiveItem usrCmp dqi >> pure True
        else lastLockTimeoutExpiredForActiveItem usrCmp dqi >> pure True

  where
    errorDequeueing e = do
      CL.logError' logCmp "Exception dequeuing" e

    gotNewActiveItem usrCmp dqi =
      catchErrorAsync "user handler" $
        CUsr.usrProcessActiveItem usrCmp dqi

    lastLockTimeoutExpiredForActiveItem usrCmp dqi = do
      catchErrorAsync "Notify work item timeout" (CUsr.usrNotifyWorkItemTimeout usrCmp dqi)
      -- pause the work item so that no other thread/process fetches it while this code is running
      void $ CR.rpPauseWorkItem repoCmp (dqi ^. C.dqaWorkItemId) 10
      -- delete the pending work item, as the pending action is done (failed)
      void $ CR.rpDeletePendingWorkItem repoCmp (dqi ^. C.dqaPendingItemId)
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
retryWorkItem repoCmp usrCmp logCmp dtCmp envCmp sys dqi = do
  wi <- CR.rpGetWorkItem repoCmp (dqi ^. C.dqaWorkItemId) >>= \case
    Left e -> UE.throwString . Txt.unpack $ "Error running retry for " <> show (dqi ^. C.dqaWorkItemId) <> "\n" <> e
    Right r -> pure r

  if wi ^. C.wiRetriesLeft > 0
    then addRetry wi
    else noMoreRetries wi

  where
    addRetry wi = do
      wt <- CEnv.envGetCachedWorkType envCmp (wi ^. C.wiWorkerTypeId) >>= \case
        Just w -> pure w
        Nothing -> do
          CR.rpGetWorkType repoCmp (wi ^. C.wiWorkerTypeId) >>= \case
            Right w -> do
              CEnv.envCacheWorkType envCmp w
              pure w
            Left e ->
              UE.throwString . Txt.unpack $ "Error fetching work type for retry\n" <> e


      now <- CDt.dtGetDate dtCmp
      let wi2 = wi & C.wiRetriesLeft %~ (`subtract` 1)
                   & C.wiAttempts %~ (+ 1)
                   & C.wiIgnoreUntil ?~ DT.addUTCTime 10 now

      -- Update the work item
      CR.rpUpdateWorkItemForRetry repoCmp wi2

      -- Create a pending item for the work item
      piid <- CR.rpCreatePendingWorkItem repoCmp (wi ^. C.wiId) >>= \case
        Right p -> pure p
        Left e -> undefined

      -- Queue to make it active but locked for the backoff period
      let backoffUntil = DT.addUTCTime (getBackoff (wt ^. C.wtDefaultBackoffSeconds) (wi ^. C.wiAttempts) 120) now

      _ <- CR.rpCreateQueueItem repoCmp piid backoffUntil

      pass -- TODO undefined

    noMoreRetries wi = do
      CR.rpDeleteWorkItem repoCmp $ wi ^. C.wiId
      pass --TODO undefined


getBackoff :: [Int] -> Int -> Int -> NominalDiffTime
getBackoff bs bAt bDefault' =
  let bDefault = fromMaybe bDefault' $ maximumMay bs in
  fromIntegral $ fromMaybe bDefault (atMay bs bAt)



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

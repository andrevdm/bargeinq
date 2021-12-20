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
import qualified Data.UUID as UU
import           Control.Lens ((^.))
import           UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Async as UA
import qualified UnliftIO.Concurrent as UC
import qualified UnliftIO.Exception as UE

import qualified BargeInQueue.Core as C
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
  -> C.SystemConfig
  -> CQ.QueueCmp m
newQueueCmpIO pgCmp lgCmp repoCmp envCmp sys = do
  let (C.SystemId sysId) = sys ^. C.sysId
  let chan = CPg.ChanName $ "c" <> Txt.replace "-" "" (UU.toText sysId)
  CQ.QueueCmp
    { CQ.qQueueWork = queueWork
    , CQ.qStartQueue = startQueue sys pgCmp lgCmp repoCmp envCmp chan
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
  -> CPg.ChanName
  -> m ()
startQueue sys pgCmp logCmp repoCmp envCmp chanName = do
  pollGate <- Th.newOpenGate

  CPg.pgListenForNotifications pgCmp chanName $ \n -> do
    CL.logTest' logCmp "LISTEN> " n
    Th.openGate pollGate

  void . UA.async $ runPollLoop pollGate (tryProcessNextActiveItem repoCmp envCmp logCmp sys)

  CL.logDebug logCmp $ "Starting poll: " <> show (sys ^. C.sysPollPeriodSeconds) <> " seconds"
  void . UA.async $ runTriggerPoll (sys ^. C.sysPollPeriodSeconds) pollGate


-- | See if there is actually an item to work with
tryProcessNextActiveItem
  :: forall m.
     (MonadUnliftIO m)
  => CR.RepoCmp m
  -> CEnv.EnvCmp m
  -> CL.LogCmp m
  -> C.SystemConfig
  -> m Bool
tryProcessNextActiveItem repoCmp envCmp logCmp sys = do
  putText "tryGetActive"
  usrCmp <- CEnv.envDemandUser envCmp

  CR.rpFetchNextActiveItem repoCmp sys >>= \case
    Right Nothing -> pure False -- Nothing was returned
    Left e -> errorDequeueing e >> pure False -- Return false in case there is a DB error. Returning True could end in an error loop

    -- If the item already had dqaDequeuedAt set then it was previously dequeued
    -- The only way it could be returned here is if the lock period expired
    -- i.e. if it timed out
    Right (Just qi) -> do
      if isNothing (qi ^. CR.dqaDequeuedAt)
        then gotNewActiveItem usrCmp qi >> pure True
        else lastLockTimeoutExpiredForActiveItem usrCmp qi >> pure True

  where
    errorDequeueing e = do
      CL.logError' logCmp "Exception dequeuing" e

    gotNewActiveItem usrCmp qi =
      catchUserErrorAsync "user handler" $
        CUsr.usrProcessActiveItem usrCmp (qi ^. CR.dqaQueueId) (qi ^. CR.dqaWorkItemId) (qi ^. CR.dqaWorkTypeId) (qi ^. CR.dqaWorkItemName)

    lastLockTimeoutExpiredForActiveItem usrCmp qi = do
      CL.logDebug logCmp "!!expire"
      catchUserErrorAsync "Notify work item timeout" (CUsr.usrNotifyWorkItemTimeout usrCmp (qi ^. CR.dqaQueueId) (qi ^. CR.dqaWorkItemId) (qi ^. CR.dqaWorkTypeId) (qi ^. CR.dqaWorkItemName))
      void $ CR.rpPauseWorkItem repoCmp (qi ^. CR.dqaWorkItemId) 5
      CR.rpDeletePendingWorkItem repoCmp (qi ^. CR.dqaPendingItemId)
      --TODO retryWorkItem repoCmp usrCmp logCmp sys (qi ^. CR.dqaWorkItemId)

    catchUserError n f =
      UE.catch f (\(e :: SomeException) -> CL.logError' logCmp ("Exception running " <> n) e)

    catchUserErrorAsync n f = void . UA.async $ catchUserError n (void f)



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

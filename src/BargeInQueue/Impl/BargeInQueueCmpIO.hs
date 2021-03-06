{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module BargeInQueue.Impl.BargeInQueueCmpIO
    ( newBargeInQueueCmpIO
    ) where

import           Verset
import           Control.Lens ((^.))
import qualified Data.Text as Txt
import           UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Exception as UE

import qualified BargeInQueue.Core as C
import qualified BargeInQueue.Components.BargeInQueueCmp as CBq
import qualified BargeInQueue.Components.EnvCmp as CEnv
import qualified BargeInQueue.Components.QueueCmp as CQ
import qualified BargeInQueue.Components.DateCmp as CDt
import qualified BargeInQueue.Components.PsqlCmp as CPg
import qualified BargeInQueue.Components.LogCmp as CL
import qualified BargeInQueue.Components.UserCmp as CUsr
import qualified BargeInQueue.Components.UuidCmp as CUu
import qualified BargeInQueue.Components.RepoCmp as CR

newBargeInQueueCmpIO
  :: forall m.
     (MonadUnliftIO m)
  => CQ.QueueCmp m
  -> CDt.DateCmp m
  -> CUu.UuidCmp m
  -> CL.LogCmp m
  -> CPg.PsqlCmp m
  -> CEnv.EnvCmp m
  -> CR.RepoCmp m
  -> CBq.BargeInQueueCmp m
newBargeInQueueCmpIO qCmp _dtCmp _uuCmp logCmp _pgCmp envCmp repoCmp =
  CBq.BargeInQueueCmp
    { CBq.bqVersion = "TODO"
    , CBq.bqAddPendingWorkItem = CR.rpAddPendingWorkItem repoCmp
    , CBq.bqAddActiveQueueItem = CR.rpAddActiveQueueItem repoCmp

    , CBq.bqStartQueue = \usrCmp -> do
        -- Flag as started
        CEnv.envSetStarted envCmp usrCmp
        -- Tell the user we are about to start
        CUsr.usrQueueStarting usrCmp
        -- Actually start
        _ <- CQ.qStartQueue qCmp
        pass

    , CBq.bqAbortWorkItem = \wiid -> do
        CL.logInfo' logCmp "User manually aborted work item" wiid
        CR.rpAbortWorkItem repoCmp wiid >>= \case
          Left e -> CL.logError' logCmp ("Error aborting workitem item: " <> show wiid) e
          Right _ -> do
            CQ.qCheckUnblocked qCmp
            CQ.qTriggerPoll qCmp

    , CBq.bqSetWorkItemDone = \wiid -> do
        usrCmp <- CEnv.envDemandUser envCmp

        wi <- CR.rpGetWorkItem repoCmp wiid >>= \case
          Left e -> UE.throwString . Txt.unpack $ "Error getting work item: " <> show wiid <> "\n" <> e
          Right r -> pure r

        CR.rpDeleteWorkItem repoCmp wiid >>= \case
          Left e -> CL.logError' logCmp ("Error deleting work item: " <> show wi) e
          Right _ -> do
            CQ.qCheckUnblocked qCmp
            CUsr.usrNotifyWorkItemSucceeded usrCmp wi
            CQ.qTriggerPoll qCmp

    , CBq.bqFailQueueItem = \qi -> do
        CL.logInfo' logCmp "User manually failed queue item" qi
        CR.rpFailQueueItem repoCmp qi >>= \case
          Left e -> CL.logError' logCmp ("Error manually failing queue item: " <> show qi) e
          Right _ -> do
            CQ.qCheckUnblocked qCmp
            CQ.qTriggerPoll qCmp

    , CBq.bqExpireQueueItem = \qi -> do
        CL.logInfo' logCmp "User manually expired queue item" qi
        CR.rpExpireQueueItem repoCmp qi >>= \case
          Left e -> CL.logError' logCmp ("Error manually expiring queue item: " <> show qi) e
          Right _ -> do
            CQ.qCheckUnblocked qCmp
            CQ.qTriggerPoll qCmp

    , CBq.bqListUnqueuedUnblockedWorkItems = do
        let sysId = CEnv.envSystem envCmp ^. C.sysId
        CR.rpListUnqueuedUnblockedWorkItems repoCmp sysId

    , CBq.bqTriggerPoll =
        CQ.qTriggerPoll qCmp

    , CBq.bqQueueAllUnblockedWorkItems = do
        let sysId = CEnv.envSystem envCmp ^. C.sysId
        CR.rpQueueAllUnblockedWorkItems repoCmp sysId

    , CBq.bqExtendTimeout = \qid until -> void $ CR.rpExtendTimeout repoCmp qid until
    , CBq.bqGotHeartBeat = CR.rpGotHeartbeat repoCmp

    , CBq.bqListSystems = CR.rpListSystems repoCmp
    , CBq.bqGetSystem = CR.rpGetSystem repoCmp
    , CBq.bqGetQueueItem = CR.rpGetQueueItem repoCmp
    , CBq.bqGetWorkItem = CR.rpGetWorkItem repoCmp
    , CBq.bqGetWorkType = CR.rpGetWorkType repoCmp

    , CBq.bqLog = pure logCmp
    }


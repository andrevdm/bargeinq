{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Lib
    ( run
    ) where

import           Verset
import           Control.Lens ((^.), (^..), traversed)
--import qualified Data.Time as DT
import qualified Data.Text as Txt
import qualified Data.UUID as UU
import qualified Data.UUID.V4 as UU
import qualified System.IO as IO
import           UnliftIO (MonadUnliftIO, throwString)

import qualified BargeInQueue.Core as C
import qualified BargeInQueue.Components.BargeInQueueCmp as CBq
import qualified BargeInQueue.Components.LogCmp as CL
import qualified BargeInQueue.Components.UserCmp as CUsr
import qualified BargeInQueue.Impl.PsqlCmpIO as CPg
import qualified BargeInQueue as Bq


testSysId :: C.SystemId
testSysId = C.SystemId $ UU.fromWords 918212935 1131432256 2699803656 3287151122

testWorkType :: C.WorkTypeId
testWorkType = C.WorkTypeId $ UU.fromWords 3523928252 3368372076 2491820724 2868489993


run :: IO ()
run = do
  --let usrCmp = newUserCmpDemo

  bq <- Bq.mkBargeInQueue
          testSysId
          "postgres://bargeinq@127.0.0.1:5432/bargeinq?sslmode=disable&options=--search_path%3dpublic"
          CPg.TraceNone -- Standard
          CL.LevelDebug
  let usrCmp = newUserCmpDemo @IO bq
  CBq.bqStartQueue bq usrCmp

  pend1Id <- C.WorkItemId <$> UU.nextRandom
  active1Id <- C.WorkItemId <$> UU.nextRandom
  wt <- CBq.bqGetWorkType bq testWorkType >>= \case
    Right r -> pure r
    Left e -> throwString . Txt.unpack $ "Error getting work type:\n" <> e

  let toPending = C.NewWorkItem
         { C._nwiId = pend1Id
         , C._nwiName = "pending1"
         , C._nwiSystemId = testSysId
         , C._nwiWorkType = wt
         , C._nwiGroupId = Nothing
         , C._nwiDependsOnWorkItem = []
         , C._nwiOverrideIgnoreUntil = Nothing
         , C._nwiOverrideRetriesLeft = Nothing
         , C._nwiOverrideBackoffSeconds = Nothing
         , C._nwiOverrideExecEnv = Nothing
         , C._nwiPriority = 1
         , C._nwiWorkData = "toPend"
         }

  wiid1 <- CBq.bqAddPendingWorkItem bq toPending >>= \case
    Right r -> pure r
    Left e -> throwString . Txt.unpack $ "Error adding pending item:\n" <> e

  print wiid1

  let toActive = C.NewWorkItem
         { C._nwiId = active1Id
         , C._nwiName = "active1"
         , C._nwiSystemId = testSysId
         , C._nwiWorkType = wt
         , C._nwiGroupId = Nothing
         , C._nwiDependsOnWorkItem = [wiid1]
         , C._nwiOverrideIgnoreUntil = Nothing
         , C._nwiOverrideRetriesLeft = Nothing
         , C._nwiOverrideBackoffSeconds = Nothing
         , C._nwiOverrideExecEnv = Nothing
         , C._nwiPriority = 2
         , C._nwiWorkData = "toActive"
         }


  qid2 <- CBq.bqAddActiveQueueItem bq toActive >>= \case
    Right r -> pure r
    Left e -> throwString . Txt.unpack $ "Error adding active item:\n" <> e

  print qid2

  putText "\n\n----------------------"
  loop

  where
    loop = do
      IO.getLine >>= \case
        "q" -> pass
        _ -> loop


newUserCmpDemo :: (MonadUnliftIO m) => CBq.BargeInQueueCmp m -> CUsr.UserCmp m
newUserCmpDemo bq =
  CUsr.UserCmp
    { CUsr.usrQueueStarting = putText "~~queue starting"

    , CUsr.usrProcessActiveItem = \dqi -> do
        putText $ "~~Processing item " <> show (dqi ^. C.dqaQueueId) <> ", for " <> (dqi ^. C.dqaWorkItemName) <> ", data= " <> show (dqi ^. C.dqaWorkData)
        --CBq.bqSetWorkItemDone bq (dqi ^. C.dqaWorkItemId)
        --CBq.bqFailQueueItem bq (dqi ^. C.dqaQueueId)
        --now <- liftIO DT.getCurrentTime
        --let until = DT.addUTCTime (10 * 60) now
        --CBq.bqExtendTimeout bq (dqi ^. C.dqaQueueId) until
        CBq.bqAbortWorkItem bq (dqi ^. C.dqaWorkItemId)

    , CUsr.usrNotifyWorkItemSucceeded = \wi -> do
        putText $ "~~succeeded" <> show (wi ^. C.wiId) <> ", " <> fromMaybe "" (wi ^. C.wiData)
        --void $ CBq.bqQueueAllUnblockedWorkItems bq

    , CUsr.usrNotifyWorkItemTimeout = \dqi -> do
        putText $ "~~Work item timeout " <> show (dqi ^. C.dqaQueueId) <> ", for " <> (dqi ^. C.dqaWorkItemName)

    , CUsr.usrNotifyWorkItemFailed = \dqi fr -> do
        putText $ "~~Work item failed " <> show (dqi ^. C.dqaQueueId) <> ", for " <> (dqi ^. C.dqaWorkItemName) <> ", with " <> show fr

    , CUsr.usrNotifyRetrypingWorkItem = \qid wi -> do
        putText $ "~~Retrying" <> show qid <> ", " <> fromMaybe "" (wi ^. C.wiData)

    , CUsr.usrNotifyWorkItemFailedNoMoreRetries = \wi ->
        putText $ "~~No more retries" <> show (wi ^. C.wiId) <> ", " <> fromMaybe "" (wi ^. C.wiData)

    , CUsr.usrNotifyHeartbeatsMissed = Just $ \qis ->
        putText $ "~~Heartbeats missed" <> show (qis ^.. traversed . C.qiId)

    , CUsr.usrNotifyHeartbeatsFailed = \qis ->
        putText $ "~~Heartbeats failed" <> show (qis ^.. traversed . C.qiId)
    }

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Lib
    ( run
    ) where

import           Verset
import           Control.Lens ((^.), (^..), traversed)
import qualified Data.Time as DT
import qualified Data.UUID as UU
import qualified Data.UUID.V4 as UU
import           Text.Pretty.Simple (pPrint)
import qualified System.IO as IO
import           UnliftIO (MonadUnliftIO)

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

  let toPending = C.PendingWorkItems
       [ C.NewWorkItem
         { C._nwiId = pend1Id
         , C._nwiName = "pending1"
         , C._nwiSystemId = testSysId
         , C._nwiWorkerType = testWorkType
         , C._nwiGroupId = Nothing
         , C._nwiDependsOnWorkItem = []
         , C._nwiOverrideIgnoreUntil = Nothing
         , C._nwiOverrideRetriesLeft = Nothing
         }
       ]

  let toActive = C.QueueWorkItems
       [ C.NewWorkItem
         { C._nwiId = active1Id
         , C._nwiName = "active1"
         , C._nwiSystemId = testSysId
         , C._nwiWorkerType = testWorkType
         , C._nwiGroupId = Nothing
         , C._nwiDependsOnWorkItem = []
         , C._nwiOverrideIgnoreUntil = Nothing
         , C._nwiOverrideRetriesLeft = Nothing
         }
       ]

  CBq.bqQueueWork bq toPending toActive

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
        CBq.bqFailQueueItem bq (dqi ^. C.dqaQueueId)
        --now <- liftIO DT.getCurrentTime
        --let until = DT.addUTCTime (10 * 60) now
        --CBq.bqExtendTimeout bq (dqi ^. C.dqaQueueId) until

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

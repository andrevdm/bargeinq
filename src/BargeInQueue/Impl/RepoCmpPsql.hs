{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BargeInQueue.Impl.RepoCmpPsql
    ( newRepoCmpPsql
    ) where

import           Verset hiding (log)
import           Control.Lens ((^.))
import qualified Data.Time as DT
import           Text.RawString.QQ (r)
import           UnliftIO (MonadUnliftIO)

import qualified BargeInQueue.Core as C
import qualified BargeInQueue.Components.DateCmp as CDt
import qualified BargeInQueue.Components.PsqlCmp as CPg
import qualified BargeInQueue.Components.RepoCmp as CR


newRepoCmpPsql
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> CDt.DateCmp m
  -> CR.RepoCmp m
newRepoCmpPsql pgCmp dtCmp =
  CR.RepoCmp
    { CR.rpListSystems = listSystems pgCmp
    , CR.rpGetSystem = getSystem pgCmp
    , CR.rpFetchNextActiveItem = fetchNextActiveItem pgCmp
    , CR.rpDeleteWorkItem = deleteWorkItem pgCmp
    , CR.rpDeleteQueueItem = deleteQueueItem pgCmp
    , CR.rpExpireQueueItem = failQueueItem pgCmp "expiring" (Just C.FrManualExpire)
    , CR.rpFailQueueItem = failQueueItem pgCmp "failing" (Just C.FrManualFail)
    , CR.rpPauseWorkItem = pauseWorkItem pgCmp dtCmp
    , CR.rpGetWorkItem = getWorkItem pgCmp
    , CR.rpGetWorkType = getWorkType pgCmp
    , CR.rpUpdateWorkItemForRetry = updateWorkItemForRetry pgCmp
    , CR.rpCreateQueueItem = createQueueItem pgCmp dtCmp
    , CR.rpListUnqueuedUnblockedWorkItems = listUnqueuedUnblockedWorkItems pgCmp
    , CR.rpQueueAllUnblockedWorkItems = queueAllUnblockedWorkItems pgCmp
    , CR.rpExtendTimeout = extendTimeout pgCmp
    }


extendTimeout
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> C.QueueItemId
  -> UTCTime
  -> m (Either Text ())
extendTimeout pgCmp (C.QueueItemId qid) until = do
  let sql = [r|
    update
      bq_queue
    set
      locked_until = ?
    where
      qid = ?
      and locked_until is not null
      and locked_until < ?
  |]
  CPg.pgExecute pgCmp sql (until, qid, until) "queue.extendTimeout" >>= \case
    Left e -> pure . Left $ "Exception extending timeout:\n" <> show e
    Right _ -> pure . Right $ ()



queueAllUnblockedWorkItems
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> C.SystemId
  -> m (Either Text ())
queueAllUnblockedWorkItems pgCmp (C.SystemId sysId) = do
  let sql = [r|
    select null from fn_bq_queue_all_unblocked(?)
  |]
  CPg.pgQuery pgCmp sql (CPg.Only sysId) "queue_all_unblocked" >>= \case
    Left e -> pure . Left $ "Exception queuing all unblocked :\n" <> show e
    Right (_ :: [CPg.Only CPg.Null]) -> pure . Right $ ()


listUnqueuedUnblockedWorkItems
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> C.SystemId
  -> Int
  -> m (Either Text [C.WorkItem])
listUnqueuedUnblockedWorkItems pgCmp (C.SystemId sysId) maxItems = do
  let sql = [r|
    select
        wi.wiid
      , wi.name
      , wi.wtid
      , wi.ignore_until
      , wi.retries_left
      , wi.created_at
      , wi.group_id
      , wi.backoff_count
      , wi.attempts
      , wi.work_data
    from
      vw_bq_unblocked_unqueued wi
    where
      wi.system_id = ?
    order by
      wi.created_at asc
    limit (?)
  |]
  CPg.pgQuery pgCmp sql (sysId, maxItems) "work_items.list_unblocked" >>= \case
    Left e -> pure . Left $ "Exception listing unblocked work items:\n" <> show e
    Right rs -> pure . Right $ rs <&> \(wiid, name, wtid, ignoreUntil, retriesLeft, createdAt, groupId, backoffCount, attempts, workData) ->
      C.WorkItem
        { C._wiId = C.WorkItemId wiid
        , C._wiSystemId = C.SystemId sysId
        , C._wiName = name
        , C._wiWorkerTypeId = C.WorkTypeId wtid
        , C._wiIgnoreUntil = ignoreUntil
        , C._wiRetriesLeft = retriesLeft
        , C._wiCreatedAt = createdAt
        , C._wiGroupId = C.GroupId <$> groupId
        , C._wiBackoffCount = backoffCount
        , C._wiAttempts = attempts
        , C._wiData = workData
        }


updateWorkItemForRetry
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> C.WorkItem
  -> m (Either Text ())
updateWorkItemForRetry pgCmp wi = do
  let sql = [r|
    update
      bq_work_item
    set
        retries_left = ?
      , ignore_until = ?
      , attempts = ?
    where
      wiid = ?
  |]
  let (C.WorkItemId wiid) = wi ^. C.wiId
  CPg.pgExecute pgCmp sql (wi ^. C.wiRetriesLeft, wi ^. C.wiIgnoreUntil, wi ^. C.wiAttempts, wiid) "work_item.updateForRetry" >>= \case
    Left e -> pure . Left $ "Exception updating work update for update:\n" <> show e
    Right _ -> pure . Right $ ()



createQueueItem
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> CDt.DateCmp m
  -> C.WorkItemId
  -> UTCTime
  -> m (Either Text C.QueueItemId)
createQueueItem pgCmp dtCmp (C.WorkItemId wiid) lockUntil = do
  let sql = [r|
    insert into bq_queue
      (wiid, created_at, locked_until)
    values
      (?, ?, ?)
    returning
      qid
  |]
  now <- CDt.dtGetDate dtCmp
  CPg.pgQuery pgCmp sql (wiid, now, lockUntil) "queue.create" >>= \case
    Left e -> pure . Left $ "Exception creating queue item for work item:" <> show wiid <> "\n" <> show e
    Right [CPg.Only qid] -> pure . Right . C.QueueItemId $ qid
    Right _ -> pure . Left $ "Error creating queue item: Invalid data returned"



getWorkType
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> C.WorkTypeId
  -> m (Either Text C.WorkType)
getWorkType pgCmp (C.WorkTypeId wtid) = do
  let sql = [r|
    select
        system_id
      , name
      , default_retries
      , default_backoff_seconds
      , default_exec_environment
      , dequeue_lock_period_seconds
      , heartbeat_expected_every_seconds
      , heartbeat_num_missed_for_error
    from
      bq_work_type
    where
      wtid = ?
  |]
  CPg.pgQuery pgCmp sql (CPg.Only wtid) "workType.fetch" >>= \case
    Left e -> pure . Left $ "Exception fetching work type:" <> show wtid <> "\n" <> show e
    Right [] -> pure . Left $ "Work type does not exist, fetching work type:" <> show wtid
    Right [(sysId, name, defRetries, CPg.PGArray defBackoff, defEnv, defLock, hbExpected', hbNum')] -> do
      let hb =
           case (hbExpected', hbNum') of
             (Just hbExpected, Just hbNum) ->
               Just C.HeartbeatSettings
                 { C._hbHeartbeatExpectedEverySeconds = hbExpected
                 , C._hbHeartbeatNumMissedForError = hbNum
                 }
             _ -> Nothing

      pure . Right $ C.WorkType
        { C._wtId = C.WorkTypeId wtid
        , C._wtSystemId = C.SystemId sysId
        , C._wtName = name
        , C._wtDefaultRetries = defRetries
        , C._wtDefaultBackoffSeconds = defBackoff
        , C._wtDefaultExecEnvironment = defEnv
        , C._wtDequeueLockPeriodSeconds = defLock
        , C._wtHeartbeatSettings = hb
        }
    Right _ -> pure . Left $ "Error fetching work type: Invalid data returned"


getWorkItem
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> C.WorkItemId
  -> m (Either Text C.WorkItem)
getWorkItem pgCmp (C.WorkItemId wiid) = do
  let sql = [r|
    select
        system_id
      , name
      , wtid
      , ignore_until
      , retries_left
      , created_at
      , group_id
      , backoff_count
      , attempts
      , work_data
    from
      bq_work_item
    where
      wiid = ?
  |]
  CPg.pgQuery pgCmp sql (CPg.Only wiid) "workItem.fetch" >>= \case
    Left e -> pure . Left $ "Exception fetching work item:" <> show wiid <> "\n" <> show e
    Right [] -> pure . Left $ "Work item does not exist, fetching work item:" <> show wiid
    Right [(sysId, name, wtid, ignoreUntil, retriesLeft, createdAt, groupId, backoffCount, attempts, workData)] ->
      pure . Right $ C.WorkItem
        { C._wiId = C.WorkItemId wiid
        , C._wiSystemId = C.SystemId sysId
        , C._wiName = name
        , C._wiWorkerTypeId = C.WorkTypeId wtid
        , C._wiIgnoreUntil = ignoreUntil
        , C._wiRetriesLeft = retriesLeft
        , C._wiCreatedAt = createdAt
        , C._wiGroupId = C.GroupId <$> groupId
        , C._wiBackoffCount = backoffCount
        , C._wiAttempts = attempts
        , C._wiData = workData
        }
    Right _ -> pure . Left $ "Error fetching work item: Invalid data returned"



pauseWorkItem
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> CDt.DateCmp m
  -> C.WorkItemId
  -> NominalDiffTime
  -> m (Either Text ())
pauseWorkItem pgCmp dtCmp (C.WorkItemId wiid) seconds = do
  let sql = [r|
    update
      bq_work_item
    set
      ignore_until = ?
    where
      wiid = ?
      and (ignore_until is null or ignore_until < ?)
  |]
  now <- CDt.dtGetLocalDate dtCmp
  let till = DT.addLocalTime seconds now
  CPg.pgExecute pgCmp sql (till, wiid, till) "work_item.ignore" >>= \case
    Left e -> pure . Left $ "Exception ignoring work item:\n" <> show e
    Right _ -> pure . Right $ ()


failQueueItem
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> Text
  -> Maybe C.FailReason
  -> C.QueueItemId
  -> m (Either Text ())
failQueueItem pgCmp reason frid (C.QueueItemId qid) = do
  let sql = [r|
    update
      bq_queue
    set
      frId = ?
    where
      qid = ?
  |]
  CPg.pgExecute pgCmp sql (C.failReasonToId <$> frid, qid) ("queue_item." <> reason) >>= \case
    Left e -> pure . Left $ "Exception " <> reason <> " failing item:\n" <> show e
    Right _ -> pure . Right $ ()



deleteQueueItem
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> C.QueueItemId
  -> m (Either Text ())
deleteQueueItem pgCmp (C.QueueItemId qid) = do
  let sql = [r|
    delete
    from
      bq_queue
    where
      qid = ?
  |]
  CPg.pgExecute pgCmp sql (CPg.Only qid) "queue.delete" >>= \case
    Left e -> pure . Left $ "Exception deleting queue item:\n" <> show e
    Right _ -> pure . Right $ ()



deleteWorkItem
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> C.WorkItemId
  -> m (Either Text ())
deleteWorkItem pgCmp (C.WorkItemId wiid) = do
  let sql = [r|
    delete
    from
      bq_work_item
    where
      wiid = ?
  |]
  CPg.pgExecute pgCmp sql (CPg.Only wiid) "work_item.delete" >>= \case
    Left e -> pure . Left $ "Exception deleting work item:\n" <> show e
    Right _ -> pure . Right $ ()



fetchNextActiveItem
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> C.SystemConfig
  -> m (Either Text (Maybe C.DequeuedActiveItem))
fetchNextActiveItem pgCmp sys = do
  let sql = [r|
    select
        r_qid
      , r_wiid
      , r_wtid
      , r_wi_name
      , r_dequeued_at
      , r_work_data
      , r_frid
    from
      bq_fetch_queue(?)
  |]
  let (C.SystemId sysId) = sys ^. C.sysId
  CPg.pgQuery pgCmp sql (CPg.Only sysId) "queue.dequeue" >>= \case
    Left e -> pure . Left $ "Exception dequeuing:\n" <> show e
    Right [] -> pure . Right $ Nothing
    Right [(qid, wiid, wtid, wiName, dqa, dqw, frId)] ->
      pure . Right . Just $ C.DequeuedActiveItem
        { C._dqaQueueId = C.QueueItemId qid
        , C._dqaWorkItemId = C.WorkItemId wiid
        , C._dqaWorkTypeId = C.WorkTypeId wtid
        , C._dqaWorkItemName = wiName
        , C._dqaDequeuedAt = dqa
        , C._dqaWorkData = dqw
        , C._dqaFailReason = C.failReasonFromId <$> frId
        }
    Right _ -> pure . Left $ "Error dequeuing: Invalid data returned"



listSystems
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> m (Either Text [C.SystemConfig])
listSystems pgCmp = do
  let sql = [r|
    select
        system_id
      , poll_period_seconds
      , locked_until
      , locked_by
      , max_active_items
      , auto_queue_unblocked
      , heartbeat_check_period_seconds
    from
      bq_system
  |]
  CPg.pgQuery_ pgCmp sql "systems.list" >>= \case
    Left e -> pure . Left $ "Exception listing systems:\n" <> show e
    Right rs -> pure . Right $ rs <&> \(sid, poll, lockUntil, lockedBy, maxActive, autoQueue, hb) ->
      C.SystemConfig
        { C._sysId = C.SystemId sid
        , C._sysPollPeriodSeconds = poll
        , C._sysLockedUntil = lockUntil
        , C._sysLockedBy = lockedBy
        , C._sysMaxActiveItems = maxActive
        , C._sysAutoQueueUnblocked = autoQueue
        , C._sysHeartbeatCheckPeriodSeconds = hb
        }


getSystem
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> C.SystemId
  -> m (Either Text (Maybe C.SystemConfig))
getSystem pgCmp (C.SystemId sysId) = do
  let sql = [r|
    select
        system_id
      , poll_period_seconds
      , locked_until
      , locked_by
      , max_active_items
      , auto_queue_unblocked
      , heartbeat_check_period_seconds
    from
      bq_system
    where
      system_id = ?
  |]
  CPg.pgQuery pgCmp sql (CPg.Only sysId) "systems.list" >>= \case
    Left e -> pure . Left $ "Exception getting system:\n" <> show e
    Right [] -> pure . Right $ Nothing
    Right [(sid, poll, lockUntil, lockedBy, maxActive, autoQueue, hb)] ->
      pure . Right . Just $ C.SystemConfig
        { C._sysId = C.SystemId sid
        , C._sysPollPeriodSeconds = poll
        , C._sysLockedUntil = lockUntil
        , C._sysLockedBy = lockedBy
        , C._sysMaxActiveItems = maxActive
        , C._sysAutoQueueUnblocked = autoQueue
        , C._sysHeartbeatCheckPeriodSeconds = hb
        }
    Right _ -> pure . Left $ "Error getting system: Invalid data returned"


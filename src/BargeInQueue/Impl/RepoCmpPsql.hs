{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

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
    , CR.rpDeletePendingWorkItem = deletePendingWorkItem pgCmp
    , CR.rpDeleteWorkItem = deleteWorkItem pgCmp
    , CR.rpExpireQueueItem = expireQueueItem pgCmp
    , CR.rpPauseWorkItem = pauseWorkItem pgCmp dtCmp
    , CR.rpGetWorkItem = getWorkItem pgCmp
    , CR.rpGetWorkType = getWorkType pgCmp dtCmp
    , CR.rpUpdateWorkItemForRetry = updateWorkItemForRetry pgCmp
    , CR.rpCreatePendingWorkItem = createPendingWorkItem pgCmp dtCmp
    , CR.rpCreateQueueItem = createQueueItem pgCmp dtCmp
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



createPendingWorkItem
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> CDt.DateCmp m
  -> C.WorkItemId
  -> m (Either Text C.PendingWorkItemId)
createPendingWorkItem pgCmp dtCmp (C.WorkItemId wiid) = do
  let sql = [r|
    insert into bq_pending_work_item
      (wiid, created_at)
    values
      (?, ?)
    returning
      piid
  |]
  now <- CDt.dtGetDate dtCmp
  CPg.pgQuery pgCmp sql (wiid, now) "pendingItem.create" >>= \case
    Left e -> pure . Left $ "Exception creating pending work item:" <> show wiid <> "\n" <> show e
    Right [CPg.Only piid] -> pure . Right . C.PendingWorkItemId $ piid
    Right _ -> pure . Left $ "Error creating pending work item: Invalid data returned"


createQueueItem
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> CDt.DateCmp m
  -> C.PendingWorkItemId
  -> UTCTime
  -> m (Either Text C.QueueItemId)
createQueueItem pgCmp dtCmp (C.PendingWorkItemId piid) lockUntil = do
  let sql = [r|
    insert into bq_queue
      (piid, created_at, locked_until)
    values
      (?, ?, ?)
    returning
      qid
  |]
  now <- CDt.dtGetDate dtCmp
  CPg.pgQuery pgCmp sql (piid, now, lockUntil) "queue.create" >>= \case
    Left e -> pure . Left $ "Exception creating queue item:" <> show piid <> "\n" <> show e
    Right [CPg.Only qid] -> pure . Right . C.QueueItemId $ qid
    Right _ -> pure . Left $ "Error creating queue item: Invalid data returned"



getWorkType
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> CDt.DateCmp m
  -> C.WorkTypeId
  -> m (Either Text C.WorkType)
getWorkType pgCmp dtCmp (C.WorkTypeId wtid) = do
  let sql = [r|
    select
        system_id
      , name
      , default_retries
      , default_backoff_seconds
      , default_heartbeat_check_period
      , default_exec_environment
      , dequeue_lock_period_seconds
    from
      bq_work_type
    where
      wtid = ?
  |]
  CPg.pgQuery pgCmp sql (CPg.Only wtid) "workType.fetch" >>= \case
    Left e -> pure . Left $ "Exception fetching work type:" <> show wtid <> "\n" <> show e
    Right [] -> pure . Left $ "Work type does not exist, fetching work type:" <> show wtid
    Right [(sysId, name, defRetries, CPg.PGArray defBackoff, defHeart, defEnv, defLock)] ->
      pure . Right $ C.WorkType
        { C._wtId = C.WorkTypeId wtid
        , C._wtSystemId = C.SystemId sysId
        , C._wtName = name
        , C._wtDefaultRetries = defRetries
        , C._wtDefaultBackoffSeconds = defBackoff
        , C._wtDefaultHeartbeatCheckPeriod = defHeart
        , C._wtDefaultExecEnvironment = defEnv
        , C._wtDequeueLockPeriodSeconds = defLock
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
      , depends_on_groups
      , depends_on_work_item
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
    Right [(sysId, name, wtid, ignoreUntil, retriesLeft, createdAt, groupId, dependsOnGroups, dependsOnWorkItems, backoffCount, attempts, workData)] ->
      pure . Right $ C.WorkItem
        { C._wiId = C.WorkItemId wiid
        , C._wiSystemId = C.SystemId sysId
        , C._wiName = name
        , C._wiWorkerTypeId = C.WorkTypeId wtid
        , C._wiIgnoreUntil = ignoreUntil
        , C._wiRetriesLeft = retriesLeft
        , C._wiCreatedAt = createdAt
        , C._wiGroupId = C.GroupId <$> groupId
        , C._wiDependsOnGroups = C.GroupId <$> maybe [] (\(CPg.PGArray a) -> a) dependsOnGroups
        , C._wiDependsOnWorkItem = C.WorkItemId <$> maybe [] (\(CPg.PGArray a) -> a) dependsOnWorkItems
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


expireQueueItem
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> C.QueueItemId
  -> m (Either Text ())
expireQueueItem pgCmp (C.QueueItemId qid) = do
  let sql = [r|
    update
      bq_queue
    set
      locked_until = null
    where
      qid = ?
  |]
  CPg.pgExecute pgCmp sql (CPg.Only qid) "queue_item.expire" >>= \case
    Left e -> pure . Left $ "Exception expiring queue item:\n" <> show e
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



deletePendingWorkItem
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> C.PendingWorkItemId
  -> m (Either Text ())
deletePendingWorkItem pgCmp (C.PendingWorkItemId piid) = do
  let sql = [r|
    delete
    from
      bq_pending_work_item
    where
      piid = ?
  |]
  CPg.pgExecute pgCmp sql (CPg.Only piid) "pending_work_item.delete" >>= \case
    Left e -> pure . Left $ "Exception deleting pending item:\n" <> show e
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
      , r_piid
      , r_wiid
      , r_wtid
      , r_wi_name
      , r_dequeued_at
      , r_work_data
    from
      bq_fetch_queue(?, ?)
  |]
  let (C.SystemId sysId) = sys ^. C.sysId
  CPg.pgQuery pgCmp sql (sysId, sys ^. C.sysPollPeriodSeconds) "queue.dequeue" >>= \case
    Left e -> pure . Left $ "Exception dequeuing:\n" <> show e
    Right [] -> pure . Right $ Nothing
    Right [(qid, piid, wiid, wtid, wiName, dqa, dqw)] ->
      pure . Right . Just $ C.DequeuedActiveItem
        { C._dqaQueueId = C.QueueItemId qid
        , C._dqaPendingItemId = C.PendingWorkItemId piid
        , C._dqaWorkItemId = C.WorkItemId wiid
        , C._dqaWorkTypeId = C.WorkTypeId wtid
        , C._dqaWorkItemName = wiName
        , C._dqaDequeuedAt = dqa
        , C._dqaWorkData = dqw
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
      , requires_global_lock
      , poll_period_seconds
      , locked_until
      , locked_by
    from
      bq_system
  |]
  CPg.pgQuery_ pgCmp sql "systems.list" >>= \case
    Left e -> pure . Left $ "Exception listing systems:\n" <> show e
    Right rs -> pure . Right $ rs <&> \(sid, reqLock, poll, lockUntil, lockedBy) ->
      C.SystemConfig
        { C._sysId = C.SystemId sid
        , C._sysRequiresGlobalLock = reqLock
        , C._sysPollPeriodSeconds = poll
        , C._sysLockedUntil = lockUntil
        , C._sysLockedBy = lockedBy
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
      , requires_global_lock
      , poll_period_seconds
      , locked_until
      , locked_by
    from
      bq_system
    where
      system_id = ?
  |]
  CPg.pgQuery pgCmp sql (CPg.Only sysId) "systems.list" >>= \case
    Left e -> pure . Left $ "Exception getting system:\n" <> show e
    Right [] -> pure . Right $ Nothing
    Right [(sid, reqLock, poll, lockUntil, lockedBy)] ->
      pure . Right . Just $ C.SystemConfig
        { C._sysId = C.SystemId sid
        , C._sysRequiresGlobalLock = reqLock
        , C._sysPollPeriodSeconds = poll
        , C._sysLockedUntil = lockUntil
        , C._sysLockedBy = lockedBy
        }
    Right _ -> pure . Left $ "Error getting system: Invalid data returned"


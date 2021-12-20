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
    }


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
  -> m (Either Text (Maybe CR.DequeuedActiveItem))
fetchNextActiveItem pgCmp sys = do
  let sql = [r|
    select
        r_qid
      , r_piid
      , r_wiid
      , r_wtid
      , r_wi_name
      , r_dequeued_at
    from
      bq_fetch_queue(?, ?)
  |]
  let (C.SystemId sysId) = sys ^. C.sysId
  CPg.pgQuery pgCmp sql (sysId, sys ^. C.sysPollPeriodSeconds) "queue.dequeue" >>= \case
    Left e -> pure . Left $ "Exception dequeuing:\n" <> show e
    Right [] -> pure . Right $ Nothing
    Right [(qid, piid, wiid, wtid, wiName, dqa)] ->
      pure . Right . Just $ CR.DequeuedActiveItem
        { CR._dqaQueueId = C.QueueItemId qid
        , CR._dqaPendingItemId = C.PendingWorkItemId piid
        , CR._dqaWorkItemId = C.WorkItemId wiid
        , CR._dqaWorkTypeId = C.WorkTypeId wtid
        , CR._dqaWorkItemName = wiName
        , CR._dqaDequeuedAt = dqa
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


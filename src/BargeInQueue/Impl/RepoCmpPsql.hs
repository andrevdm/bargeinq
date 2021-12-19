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
import           Text.RawString.QQ (r)
import           UnliftIO (MonadUnliftIO)

import qualified BargeInQueue.Core as C
import qualified BargeInQueue.Components.PsqlCmp as CPg
import qualified BargeInQueue.Components.RepoCmp as CR


newRepoCmpPsql
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> CR.RepoCmp m
newRepoCmpPsql pgCmp =
  CR.RepoCmp
    { CR.rpListSystems = listSystems pgCmp
    , CR.rpGetSystem = getSystem pgCmp
    , CR.rpFetchNextActiveItem = fetchNextActiveItem pgCmp
    }


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
    from
      bq_fetch_queue(?)
  |]
  CPg.pgQuery pgCmp sql (CPg.Only $ sys ^. C.sysPollPeriodSeconds) "queue.dequeue" >>= \case
    Left e -> pure . Left $ "Exception dequeuing:\n" <> show e
    Right [] -> pure . Right $ Nothing
    Right [(qid, piid, wiid, wtid, wiName)] ->
      pure . Right . Just $ CR.DequeuedActiveItem
        { CR._dqaQueueId = C.QueueItemId qid
        , CR._dqaPendingItemId = C.PendingWorkItemId piid
        , CR._dqaWorkItemId = C.WorkItemId wiid
        , CR._dqaWorkTypeId = C.WorkTypeId wtid
        , CR._dqaWorkItemName = wiName
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
        { C._sysId = sid
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
        { C._sysId = sid
        , C._sysRequiresGlobalLock = reqLock
        , C._sysPollPeriodSeconds = poll
        , C._sysLockedUntil = lockUntil
        , C._sysLockedBy = lockedBy
        }
    Right _ -> pure . Left $ "Error getting system: Invalid data returned"


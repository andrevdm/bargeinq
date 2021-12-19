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
import           UnliftIO (MonadUnliftIO)
import           Text.RawString.QQ (r)

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
    }


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


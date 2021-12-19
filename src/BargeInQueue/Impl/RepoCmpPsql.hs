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
import qualified Data.Text as Txt
import qualified Data.Time as DT
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
    , CR.rpFetchSystem = const . pure $ Nothing
    }


listSystems
  :: forall m.
     (MonadUnliftIO m)
  => CPg.PsqlCmp m
  -> m (Either SomeException [CR.SystemConfig])
listSystems pgCmp = do
  let sql = [r|
    select
        system_id
      , requires_global_lock
      , poll_period_seconds
      , locked_until
      , locked_by
    from bq_system
  |]
  CPg.pgQuery_ pgCmp sql "systems.list" >>= \case
    Left e -> pure . Left $ e
    Right rs -> pure . Right $ rs <&> \(sid, reqLock, poll, lockUntil, lockedBy) ->
      CR.SystemConfig
        { CR.scId = sid
        , CR.scRequiresGlobalLock = reqLock
        , CR.scPollPeriodSeconds = poll
        , CR.scLockedUntil = lockUntil
        , CR.scLockedBy = lockedBy
        }

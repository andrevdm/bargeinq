{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Registry
    ( mkBargeInQueue
    ) where

import           Protolude

import qualified Components.BargeInQueueCmp as CBq
import qualified Components.QueueCmp as CQ
import qualified Impl.BargeInQueueCmpIO as CBq
import qualified Impl.DateCmpIO as CDt
import qualified Impl.QueueCmpIO as CQ
import qualified Impl.PsqlCmpIO as CPg
import qualified Impl.UuidCmpIO as CUu
import qualified Impl.LogCmpIO as CL

mkBargeInQueue
  :: CQ.SystemId
  -> Text
  -> CPg.TracePg
  -> IO (CBq.BargeInQueueCmp IO)
mkBargeInQueue sysId connStr tracePg = do
  pool <- CPg.createPgConnPool connStr

  let
    dt = CDt.newDateCmpIO @IO
    uu = CUu.newUuidCmpIO @IO
    lg = CL.newLogCmpIO @IO [] dt
    pg = CPg.newPsqlCmpIO @IO tracePg pool lg
    q = CQ.newQueueCmpIO @IO
    bq = CBq.newBargeInQueueCmpIO q dt uu lg pg
  _ <- CQ.qStartQueue q sysId
  pure bq

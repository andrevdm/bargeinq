{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BargeInQueue
    ( mkBargeInQueue
    ) where

import           Verset
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBMQueue as TBMQ

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
  prnQ <- atomically $ TBMQ.newTBMQueue 1000
  let termWriter = CL.createQueueLogWriter prnQ
  CL.startTerminalPrinter prnQ

  let dt = CDt.newDateCmpIO @IO
  let uu = CUu.newUuidCmpIO @IO
  let lg = CL.newLogCmpIO @IO [termWriter] dt
  pg <- CPg.newPsqlCmpIO @IO tracePg connStr lg
  let q = CQ.newQueueCmpIO @IO pg lg
  let bq = CBq.newBargeInQueueCmpIO q dt uu lg pg

  _ <- CQ.qStartQueue q sysId

  pure bq

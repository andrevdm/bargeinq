{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BargeInQueue
    ( mkBargeInQueue
    ) where

import           Verset
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBMQueue as TBMQ
import           Text.Pretty.Simple (pPrint)


import qualified BargeInQueue.Core as C
import qualified BargeInQueue.Components.BargeInQueueCmp as CBq
import qualified BargeInQueue.Components.RepoCmp as CR
import qualified BargeInQueue.Components.QueueCmp as CQ
import qualified BargeInQueue.Impl.BargeInQueueCmpIO as CBq
import qualified BargeInQueue.Impl.DateCmpIO as CDt
import qualified BargeInQueue.Impl.LogCmpIO as CL
import qualified BargeInQueue.Impl.PsqlCmpIO as CPg
import qualified BargeInQueue.Impl.QueueCmpIO as CQ
import qualified BargeInQueue.Impl.RepoCmpPsql as CR
import qualified BargeInQueue.Impl.UuidCmpIO as CUu

mkBargeInQueue
  :: C.SystemId
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
  let repo = CR.newRepoCmpPsql pg
  let q = CQ.newQueueCmpIO @IO pg lg sysId
  let bq = CBq.newBargeInQueueCmpIO q dt uu lg pg

  ss <- CR.rpListSystems repo
  pPrint ss

  _ <- CQ.qStartQueue q

  pure bq

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module BargeInQueue
    ( mkBargeInQueue
    ) where

import           Verset
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBMQueue as TBMQ
import           Control.Lens ((^.))
import qualified Data.Text as Txt
import           UnliftIO.Exception (throwString)


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
  -- Logging
  prnQ <- atomically $ TBMQ.newTBMQueue 1000
  let termWriter = CL.createQueueLogWriter prnQ
  CL.startTerminalPrinter prnQ

  -- Create enough components so we can query the repo
  let dt = CDt.newDateCmpIO @IO
  let lg = CL.newLogCmpIO @IO [termWriter] dt
  pg <- CPg.newPsqlCmpIO @IO tracePg connStr lg
  let repo = CR.newRepoCmpPsql pg

  -- Fetch the system config
  sysConfig <- CR.rpGetSystem repo sysId >>= \case
    Left e -> throwString $ Txt.unpack e
    Right Nothing -> throwString $ "BargeInQueue system does not exist: " <> show sysId
    Right (Just s) ->
      if s ^. C.sysId == sysId
        then pure s
        else throwString $ "Invalid SystemId returned. Expecting: " <> show sysId <> ", got: " <> show (s ^. C.sysId)

  -- Create the rest
  let uu = CUu.newUuidCmpIO @IO
  let q = CQ.newQueueCmpIO @IO pg lg repo sysConfig
  let bq = CBq.newBargeInQueueCmpIO q dt uu lg pg


  -- Start the queue
  _ <- CQ.qStartQueue q

  pure bq

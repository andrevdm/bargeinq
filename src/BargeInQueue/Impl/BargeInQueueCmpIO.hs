{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module BargeInQueue.Impl.BargeInQueueCmpIO
    ( newBargeInQueueCmpIO
    ) where

import           Verset

import qualified BargeInQueue.Components.BargeInQueueCmp as CBq
import qualified BargeInQueue.Components.EnvCmp as CE
import qualified BargeInQueue.Components.QueueCmp as CQ
import qualified BargeInQueue.Components.DateCmp as CDt
import qualified BargeInQueue.Components.PsqlCmp as CPg
import qualified BargeInQueue.Components.LogCmp as CL
import qualified BargeInQueue.Components.UserCmp as CUsr
import qualified BargeInQueue.Components.UuidCmp as CUu
import qualified BargeInQueue.Components.RepoCmp as CR

newBargeInQueueCmpIO
  :: forall m.
     (Monad m)
  => CQ.QueueCmp m
  -> CDt.DateCmp m
  -> CUu.UuidCmp m
  -> CL.LogCmp m
  -> CPg.PsqlCmp m
  -> CE.EnvCmp m
  -> CR.RepoCmp m
  -> CBq.BargeInQueueCmp m
newBargeInQueueCmpIO qCmp _dtCmp _uuCmp logCmp _pgCmp envCmp repoCmp =
  CBq.BargeInQueueCmp
    { CBq.bqVersion = "TODO"
    , CBq.bqQueueWork = CQ.qQueueWork qCmp

    , CBq.bqStartQueue = \usrCmp -> do
        -- Flag as started
        CE.envSetStarted envCmp usrCmp
        -- Tell the user we are about to start
        CUsr.usrQueueStarting usrCmp
        -- Actually start
        _ <- CQ.qStartQueue qCmp
        pass

    , CBq.bqSetWorkItemDone = \wi -> do
        CR.rpDeleteWorkItem repoCmp wi >>= \case
          Right _ -> pass
          Left e -> CL.logError' logCmp ("Error deleting work item: " <> show wi) e

    , CBq.bqFailQueueItem = \qi -> do
        CL.logInfo' logCmp "User manually failed queue item" qi
        CR.rpFailQueueItem repoCmp qi >>= \case
          Right _ -> pass
          Left e -> CL.logError' logCmp ("Error manually failing queue item: " <> show qi) e

    , CBq.bqExpireQueueItem = \qi -> do
        CL.logInfo' logCmp "User manually expired queue item" qi
        CR.rpExpireQueueItem repoCmp qi >>= \case
          Right _ -> pass
          Left e -> CL.logError' logCmp ("Error manually expiring queue item: " <> show qi) e
    }


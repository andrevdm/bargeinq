{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module BargeInQueue.Impl.BargeInQueueCmpIO
    ( newBargeInQueueCmpIO
    ) where

import           Verset

import qualified BargeInQueue.Components.BargeInQueueCmp as CBq
import qualified BargeInQueue.Components.QueueCmp as CQ
import qualified BargeInQueue.Components.DateCmp as CDt
import qualified BargeInQueue.Components.PsqlCmp as CPg
import qualified BargeInQueue.Components.UuidCmp as CUu
import qualified BargeInQueue.Components.LogCmp as CL

newBargeInQueueCmpIO
  :: forall m.
     (Monad m)
  => CQ.QueueCmp m
  -> CDt.DateCmp m
  -> CUu.UuidCmp m
  -> CL.LogCmp m
  -> CPg.PsqlCmp m
  -> CBq.BargeInQueueCmp m
newBargeInQueueCmpIO qCmp dtCmp uuCmp lgCmp pgCmp =
  CBq.BargeInQueueCmp
    { CBq.bqQueueWork = CQ.qQueueWork qCmp
    }


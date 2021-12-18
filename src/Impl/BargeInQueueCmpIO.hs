{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Impl.BargeInQueueCmpIO
    ( newBargeInQueueCmpIO
    ) where

import           Protolude

import qualified Components.BargeInQueueCmp as CBq
import qualified Components.QueueCmp as CQ
import qualified Components.DateCmp as CDt
import qualified Components.PsqlCmp as CPg
import qualified Components.UuidCmp as CUu
import qualified Components.LogCmp as CL

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


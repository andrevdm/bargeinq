{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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

newBargeInQueueCmpIO
  :: forall m.
     (Monad m)
  => CQ.QueueCmp m
  -> CDt.DateCmp m
  -> CUu.UuidCmp m
  -> CL.LogCmp m
  -> CPg.PsqlCmp m
  -> CE.EnvCmp m
  -> CBq.BargeInQueueCmp m
newBargeInQueueCmpIO qCmp _dtCmp _uuCmp _lgCmp _pgCmp envCmp =
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
    }


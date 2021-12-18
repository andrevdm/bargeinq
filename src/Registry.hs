{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Registry
    ( mkQueue
    ) where

import           Protolude

import qualified Components.QueueCmp as CQ
import qualified Impl.DateCmpIO as CDt
import qualified Impl.QueueCmpIO as CQ
import qualified Impl.PsqlCmpIO as CPg
import qualified Impl.UuidCmpIO as CUu
import qualified Impl.LogCmpIO as CL

mkQueue :: IO (CQ.QueueCmp IO)
mkQueue = do
  let
    dt = CDt.newDateCmpIO @IO
    uu = CUu.newUuidCmpIO @IO
    lg = CL.newLogCmpIO @IO [] dt
    pg = CPg.newPsqlCmpIO @IO True lg
    q = CQ.newQueueCmpIO @IO
  pure q

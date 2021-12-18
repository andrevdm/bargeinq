{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Registry
    ( mkApp
    ) where

import           Protolude

import qualified Components.AppCmp as CA
import qualified Components.QueueCmp as CQ
import qualified Impl.AppCmpIO as CA
import qualified Impl.DateCmpIO as CDt
import qualified Impl.QueueCmpIO as CQ
import qualified Impl.PsqlCmpIO as CPg
import qualified Impl.UuidCmpIO as CUu
import qualified Impl.LogCmpIO as CL

mkApp :: CQ.SystemId -> IO (CA.AppCmp IO)
mkApp sysId = do
  let
    dt = CDt.newDateCmpIO @IO
    uu = CUu.newUuidCmpIO @IO
    lg = CL.newLogCmpIO @IO [] dt
    pg = CPg.newPsqlCmpIO @IO True lg
    q = CQ.newQueueCmpIO @IO
    app = CA.newAppCmpIO q dt uu lg pg
  _ <- CQ.qStartQueue q sysId
  pure app

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Registry
    ( mkApp
    ) where

import           Protolude

import qualified Components.AppCmp as CA
import qualified Impl.AppCmpIO as CA
import qualified Impl.DateCmpIO as CDt
import qualified Impl.QueueCmpIO as CQ
import qualified Impl.PsqlCmpIO as CPg
import qualified Impl.UuidCmpIO as CUu
import qualified Impl.LogCmpIO as CL

mkApp :: IO (CA.AppCmp IO)
mkApp = do
  let
    q = CQ.newQueueCmpIO @IO
    dt = CDt.newDateCmpIO @IO
    uu = CUu.newUuidCmpIO @IO
    lg = CL.newLogCmpIO @IO [] dt
    pg = CPg.newPsqlCmpIO @IO True lg
  pure $ CA.newAppCmpIO @IO q dt uu lg pg

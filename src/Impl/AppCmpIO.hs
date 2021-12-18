{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Impl.AppCmpIO
    ( newAppCmpIO
    ) where

import           Protolude

import qualified Components.AppCmp as CA
import qualified Components.QueueCmp as CQ
import qualified Components.DateCmp as CDt
import qualified Components.PsqlCmp as CPg
import qualified Components.UuidCmp as CUu
import qualified Components.LogCmp as CL

newAppCmpIO
  :: forall m.
     (Monad m)
  => CQ.QueueCmp m
  -> CDt.DateCmp m
  -> CUu.UuidCmp m
  -> CL.LogCmp m
  -> CPg.PsqlCmp m
  -> CA.AppCmp m
newAppCmpIO qCmp dtCmp uuCmp lgCmp pgCmp =
  CA.AppCmp
    { CA.appRunQueue = CQ.qStartQueue qCmp
    }

